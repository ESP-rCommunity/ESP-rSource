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
C  EASKS248: Edit a long (248 char) string in sections.
C  EASKSCMD Ask user for a string with prompt, alt, error messages & help.
C  EASKSCNCL Asking the user for a text string with prompt cancel error & help.
C  EASKS2CMD Ask user for a string with prompt, 2 alts, error messages & help.

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
C  PHELPD:  Displays the current contents of common pophelp.
C  PHELPW:  Returns the width IWH of the longest popup help string.

C  EVSET:   provides setup environment for wire frame view w/in prj.
C Startbuffer: wrapper around call to win3dclr
C popupimage: display image with documentation (dummy of GTK version)

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
C to identify the value.
C IER is the error state, if 0 then OK, if -3 then cancel pressed.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C Examples:
C Ask user for zone number.
C        ICOMP=1
C        CALL EASKI(ICOMP,' ','Zone number?',
C     &             1,'F',MCOM,'W',1,'zone number',IER,2)

      SUBROUTINE EASKI(IVAL,PROMP1,PROMP2,MINV,MINACT,
     &                 MAXV,MAXACT,IDEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*10,STR1*10,STR2*10
      CHARACTER ask*10,OUTSTR*124,DSTR*124,dh*72,cancel*6,outs*124
      LOGICAL OK,DOK
      integer ivalold

      IER=0

C If IVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.IVAL.LT.MINV)IVAL=IDEFLT
      if(MAXACT.EQ.'F'.and.IVAL.GT.MAXV)IVAL=IDEFLT

C Clear string buffers and instantiate 'ask'
   20 WORD=' '
      WRITE(ask,'(I5)',iostat=ios,err=1)IVAL

C Remember value passed in in case of a cancel.
      ivalold=ival

C Generate help and default strings.
      CALL INTSTR(IDEFLT,STR2,IW2,IER)
      WRITE(DSTR,27,iostat=ios,err=1)STR2(1:IW2)
   27 FORMAT('The default value (',a,') will be used.')

      CALL INTSTR(IVAL,STR2,IW2,IER)
      CALL INTSTR(IDEFLT,STR1,IW1,IER)
      call helpcurrentint(ideflt,ival,ermsg,nhelp,newnbhelp,ier)

C Begin with prompts.
      IF(MMOD.EQ.8)THEN

C If in graphic mode use graphic faciltiies to edit the number.
        idef=0
        iquery=0
        iwidth=10

C Include a cancel option.
        cancel='cancel'
        CALL openaskcnclbox(PROMP1,PROMP2,cancel,iwidth)
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        CALL askcncldialog(ask,cancel,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(ask,WORD)
        ask='        '
        if(idef.EQ.1)then

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,DSTR)
          IVAL=IDEFLT
C          call usrmsg('  ','  ','-')
          RETURN
        elseif(idef.eq.2)then

C Cancel button was pressed, reset ival as ivalold and mark as cancel. 
          IVAL=ivalold
C          call usrmsg('User requested a cancel for dialog',PROMP1,'-')
          ier=-3
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
            CALL PHELPD('ask user integer',newnbhelp,'-',0,0,IER)
            GOTO 20
          ENDIF
        ELSEIF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing integer value.
          CALL INTSTR(IVAL,STR2,IW2,IER)
          WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55     FORMAT('The existing value (',a,') will be used.')
          RETURN
        ENDIF
      ENDIF

C Decode the string into an integer.
      read(WORD,*,ERR=999)IVALT
      CALL INTSTR(IVALT,STR1,IW1,IFLAG)
      IF(IFLAG.NE.0)THEN
        CALL USRMSG(' ','Did not understand, try again.','-')
        IFLAG=0
        GOTO 20
      ENDIF

C Check IVALT against minimum and respond based on MINACT.
      IF(IVALT.LT.MINV)THEN
        CALL INTSTR(MINV,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,28,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   28   FORMAT('The input value for `',a,'` (',a,
     &         ') should be greater than ',a,'!')
        IF(MINACT.EQ.'W')THEN
          DOK=.true.
          CALL ASKOK(OUTSTR,'Is this OK?',OK,DOK,newnbhelp)
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
   39   FORMAT('The input value for `',a,'` (',a,')',
     &         ' should be less than ',a,'!')
        IF(MAXACT.EQ.'W')THEN
          DOK=.true.
          CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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

999   CALL USRMSG(' ','Did not understand, try again.','-')
      GOTO 20

   1  if(IOS.eq.2)then
        write(outs,*) 'EASKI: permission error composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKI: error composing message or prompt.'
        call edisp(iuout,outs)
      endif
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
C to identify the value.
C IER is the error state, if 0 then OK, if -3 then cancel button pressed.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C Example:
C    ....
C Read a site latitude.
C        XLAT=50.0
C        CALL EASKR(XLAT,' ',' What is the site latitude (deg) ? ',
C     &             -89.9,'F',89.9,'F',35.0,'site latitude',IER,3)
C    ...

      SUBROUTINE EASKR(RVAL,PROMP1,PROMP2,RMIN,MINACT,
     &                 RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*16,STR2*16
      CHARACTER ask*16,OUTSTR*124,DSTR*124,dh*72,outs*124
      LOGICAL OK,close,DOK

C If RVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.RVAL.LT.RMIN)RVAL=DEFLT
      if(MAXACT.EQ.'F'.and.RVAL.GT.RMAX)RVAL=DEFLT

C Check that value is not trivially close to zero (i.e. not
C instantiated on the Fortran side before the call).
      CALL ECLOSE(RVAL,0.00,0.000001,CLOSE)
      if(close)RVAL=0.0
   20 WORD=' '
      if(abs(rval).gt.1.E+6)then
        WRITE(ask,'(1PE14.6)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.1.E+6.and.rval.gt.1.E+4)then
        WRITE(ask,'(G15.5)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.1.E+4.and.rval.gt.1.E-3)then
        WRITE(ask,'(G14.4)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.1.E-3.and.rval.gt.1.E-4)then
        WRITE(ask,'(F10.7)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.1.E-4)then
        WRITE(ask,'(1PE14.6)',IOSTAT=ios,ERR=999)RVAL
      else
        WRITE(ask,'(F14.4)',IOSTAT=ios,ERR=999)RVAL
      endif

C Generate help and default strings.
      CALL REL16STR(DEFLT,STR2,IW2,IER)
      WRITE(DSTR,27,iostat=ios,err=1)STR2(1:IW2)
   27 FORMAT('The default value (',a,') will be used.')

      CALL REL16STR(DEFLT,STR1,IW1,IER)
      CALL REL16STR(RVAL,STR2,IW2,IER)
      call helpcurrentreal(deflt,rval,ermsg,nhelp,newnbhelp,ier)

C Begin with prompts.
      IF(MMOD.EQ.8)THEN

C If in graphic mode use graphic faciltiies to edit the number.
        idef=0
        iquery=0
        iwidth=16

C << Future addition of a cancel option here. >>
        CALL openaskbox(PROMP1,PROMP2,iwidth)
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        CALL askdialog(ask,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(ask,WORD)
        IF(idef.EQ.1)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,DSTR)
          RVAL=DEFLT
C          call usrmsg('  ','  ','-')
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
            CALL PHELPD('ask user real',newnbhelp,'-',0,0,IER)
            GOTO 20
          endif
        ELSEIF(WORD.EQ.' ')THEN

C A carriage return or line feed encountered, assume user wishes to
C use prior/existing real value.
          CALL REL16STR(RVAL,STR2,IW2,IER)
          WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55     FORMAT('The existing value (',a,') will be used.')
          RETURN
        ENDIF
      ENDIF

C Decode the string into an real.
      read(WORD,*,ERR=89)rvalt
      CALL REL16STR(RVALT,STR1,IW1,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ','Did not understand, try again.','-')
        IER=0
        GOTO 20
      ENDIF

C Check RVALT against minimum and respond based on MINACT.
      IF(RVALT.LT.RMIN)THEN
        CALL REL16STR(RMIN,STR2,IW2,IER)
        LN=max(1,LNBLNK(ERMSG))
        WRITE(OUTSTR,28,iostat=ios,err=1)ERMSG(1:LN),
     &    STR1(1:IW1),STR2(1:IW2)
   28   FORMAT('The input value for `',a,'` (',a,
     &         ') should be greater than ',a,'!')
        IF(MINACT.EQ.'W')THEN
          DOK=.false.
          CALL ASKOK(OUTSTR,'Is this OK?',OK,DOK,newnbhelp)
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
   39   FORMAT('The input value for `',a,'` (',a,
     &         ')',' should be less than ',a,'!')
        IF(MAXACT.EQ.'W')THEN
          DOK=.false.
          CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
          RVAL=RVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MAXACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter.','W')
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
 89   CALL USRMSG(' ','Did not understand, try again.','-')
      GOTO 20

   1  if(IOS.eq.2)then
        write(outs,*) 'EASKR: permission error composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKR: error composing message or prompt.'
        call edisp(iuout,outs)
      endif
      return
 999  if(IOS.eq.2)then
        WRITE(outs,*) 'EASKR: write permission or invalid real: ',RVAL
        call edisp(iuout,outs)
      else
        WRITE(outs,*) 'EASKR: invalid real or > 16 chars: ',RVAL
        call edisp(iuout,outs)
      endif
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
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.

      SUBROUTINE EASKE(RVAL,PROMP1,PROMP2,RMIN,MINACT,
     &                 RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*10,STR2*10
      CHARACTER ask*16,OUTSTR*124,DSTR*124,dh*72,outs*124
      LOGICAL OK,close,DOK

      IER=0

C If RVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.RVAL.LT.RMIN)RVAL=DEFLT
      if(MAXACT.EQ.'F'.and.RVAL.GT.RMAX)RVAL=DEFLT

C Check that value is not trivially close to zero (ie not instantiated
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
      call helpcurrentreal(deflt,rval,ermsg,nhelp,newnbhelp,ier)

C Begin with prompts.
C If in terminal type 8 (dialogue box) use ww facilities to get the
C string to convert.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        iwidth=16
        CALL openaskbox(PROMP1,PROMP2,iwidth)
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        CALL askdialog(ask,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(ask,WORD)
        IF(idef.EQ.1)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,DSTR)
          call edisp(iuout,' ')
          RVAL=DEFLT
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
            CALL PHELPD('ask user real',newnbhelp,'-',0,0,IER)
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
          CALL ASKOK(OUTSTR,' Is this OK ? ',OK,DOK,newnbhelp)
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
          CALL ASKOK(OUTSTR,' Is this ok? ',OK,DOK,newnbhelp)
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
   1  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKE: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKE: error composing message or prompt.'
        call edisp(iuout,outs)
      endif
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
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C << this is a place holder so the X version has similar functionality
C << to the GTK version. It should be extended to support browsing etc.

      SUBROUTINE EASKF(FILEN,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,FILEN,DSTR
      CHARACTER WORD144*144,A*1,LASTS*144,OUTSTR*124,dh*72,outs*124
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

C Generate custom help text if needed.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openaskbox(PROMP1,PROMP2,ISTRW)
        WORD144=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        lmmod=mmod
        CALL askdialog(FILEN,idef,iquery)

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(FILEN,WORD144)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
     &      DSTR(1:LN),'`.'
          DOK=.true.
          CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
     &        DSTR(1:LN),'`.'
            DOK=.true.
            CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            CALL PHELPD('ask file name',newnbhelp,'-',0,0,IER)
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
   1  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKF: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKF: error composing message or prompt.'
        call edisp(iuout,outs)
      endif
      return
   2  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKF: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKF: error composing prompt.'
        call edisp(iuout,outs)
      endif
      return
   3  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKF: permissions exception while writing file name.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKF: error writing file name.'
        call edisp(iuout,outs)
      endif
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
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Internal string buffer assumes that strval is less than 96 characters.
C Example:
C    ....
C test getting a string.
C        XNAME=' '
C        DNAME=' '
C        CALL EASKS(XNAME,' ',' What is the site name ?',
C     &   13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKS(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72,outs*124
      LOGICAL OK,DOK

   20 WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)
      if (last.gt.6) then
        if(STRVAL(1:7).eq.'UNKNOWN'.or.STRVAL(1:7).eq.'unknown'.or.
     &     STRVAL(1:7).eq.'Unknown') STRVAL='  '
      endif

C Generate custom help text.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openaskbox(PROMP1,PROMP2,ISTRW)
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
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
          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
     &      DSTR(1:LN),'`.'
          DOK=.true.
          CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
     &        DSTR(1:LN),'`.'
            DOK=.true.
            CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            CALL PHELPD('ask user string',newnbhelp,'-',0,0,IER)
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
   1  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKS: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKS: error composing message or prompt.'
        call edisp(iuout,outs)
      endif
      return
   2  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKS: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKS: error composing prompt.'
        call edisp(iuout,outs)
      endif
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
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
      SUBROUTINE EASKS248(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,
     &   NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      character t72*72,t72a*72,t72b*72,t72c*72
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
      t72a=' '
      t72b=' '
      t72c=' '
      write(t72,'(a)') STRVAL(1:ip72)
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

C ************* EASKSCMD
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
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Internal buffer size assumes that STRVAL is less than 96 characters.

C Example:
C    ....
C test getting a string.
C        XNAME=' '
C        DNAME=' '
C        CALL EASKSCMD(XNAME,' ',' What is the site name ?','Use map',CACT,
C     &   13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKSCMD(STRVAL,PROMP1,PROMP2,CMD,CMDACT,ISTRW,DSTR,
     &  ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CMD,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72
      LOGICAL OK,CMDACT,DOK

   20 IER=0
      CMDACT=.FALSE.
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)

C Generate custom help text.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openaskaltbox(PROMP1,PROMP2,CMD,ISTRW)
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        CALL askaltdialog(STRVAL,CMD,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)') 'The default is `',
     &      DSTR(1:LN),'`.'
          DOK=.true.
          CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            CALL USRMSG('The current string is blank!',
     &                  'Please re-enter.','W')
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
            WRITE(OUTSTR,'(3a)') 'The default is `',
     &        DSTR(1:LN),'`.'
            DOK=.true.
            CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            CALL PHELPD('ask user string',newnbhelp,'-',0,0,IER)
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

C ************* EASKSCNCL
C EASKSCNCL is a facility for asking the user for a text string which
C incorporates the prompt, cancel, error messages and
C help facilities as follows:

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C CNCL is cancel command string, CNCLACT is logical if the
C command action is selected.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Internal buffer size assumes that STRVAL is less than 96 characters.

C Example:
C    ....
C test getting a string.
C        XNAME=' '
C        DNAME=' '
C        CALL EASKSCNCL(XNAME,' ',' What is the site name ?','Use map',CACT,
C     &   13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKSCNCL(STRVAL,PROMP1,PROMP2,CNCL,CNCLACT,ISTRW,
     &  DSTR,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CNCL,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72
      character outstr248*248
      LOGICAL OK,CNCLACT,DOK

   20 IER=0
      CNCLACT=.FALSE.
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)

C Generate custom help text.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openaskcnclbox(PROMP1,PROMP2,CNCL,ISTRW)
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        CALL askcncldialog(STRVAL,CNCL,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)') 'The default is `',
     &      DSTR(1:LN),'`.'
          DOK=.true.
          CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            RETURN
          ELSE
            GOTO 20
          ENDIF
        ENDIF
        IF(idef.EQ.2)THEN
          WRITE(OUTSTR248,55)LASTS(1:last)
          call edisp248(iuout,outstr248,90)
          write(STRVAL,'(a)') LASTS(1:last)
          CNCLACT=.TRUE.
          RETURN
        endif
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
            STRVAL=LASTS(1:last)
            CNCLACT=.FALSE.
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:ISTRW)
        CNCLACT=.FALSE.
        RETURN
      ELSE
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A72)')WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)') 'The default is `',
     &        DSTR(1:LN),'`.'
            DOK=.true.
            CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            CALL PHELPD('ask user string',newnbhelp,'-',0,0,IER)
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
C CMD & CMD2 are alternative command strings, if ICACT is non-zero
C a command action is selected.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Internal string buffer assumes that strval is less than 96 characters.
C Example:
C    ....
C test getting a string.
C        XNAME=' '
C        DNAME=' '
C        CALL EASKS2CMD(XNAME,' ','Site name ?','Use map','Use iwec',ICACT,
C     &   13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKS2CMD(STRVAL,PROMP1,PROMP2,CMD,CMD2,ICACT,ISTRW,
     &  DSTR,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CMD,CMD2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72
      LOGICAL OK,DOK

   20 IER=0
      ICACT=0
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)

C Generate custom help text.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        idef=0
        iquery=0
        CALL openask2altbox(PROMP1,PROMP2,CMD,CMD2,ISTRW)
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        CALL ask2altdialog(STRVAL,CMD,CMD2,idef,iquery)

C Debug.
C        write(6,*) 'just after ask2altdialog idef iquery',idef,iquery

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)') 'The default is `',
     &      DSTR(1:LN),'`.'
          DOK=.true.
          CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            WRITE(OUTSTR,'(3a)') 'The default is `',
     &        DSTR(1:LN),'`.'
            DOK=.true.
            CALL ASKOK(OUTSTR,'Is this ok?',OK,DOK,newnbhelp)
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
            CALL PHELPD('ask user string',newnbhelp,'-',0,0,IER)
            GOTO 20
          ELSE

C Neither help or default, could be a single character string was
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
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.

C Note that there is no particular limit on the number of alternate
C strings which can be passed to the subroutine as the selection menu
C allows paging.
C User-defined text strings and string alternatives
C will be truncated at the width ISTRW.

      SUBROUTINE EPICKS(INPICK,IVALS,PROMP1,PROMP2,
     &                ISTRW,NSTALT,STALT,ERMSG,IER,NHELP)
#include "epara.h"

      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      LOGICAL FOUND,ALL

      DIMENSION IVALS(NSTALT)
      dimension lista(35),listb(35)
      character lista*74,listb*74,KEY*1,outs*124
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STALT(*)
      CHARACTER H*72,SEL*40
      integer MVERT,IVERT  ! max items and current menu item

#ifdef OSI
      integer impx,impy,iw
#else
      integer*8 impx,impy,iw
#endif

C At this point bring up a menu with the string alternatives, assuming
C there is more than one alternative to pick from.
      IF(NSTALT.LT.1)THEN
        CALL USRMSG(ERMSG,'Number of menu items too small.','W')
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
      INPICK=0
      DO 40 I=1,NSTALT
        IVALS(I)=0
   40 CONTINUE

C Initial menu entry setup.
   92 IVERT=-3

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
  115     FORMAT   ('0 Page part: ',I2,' of ',I2)
        ELSEIF(ISTRW.GT.28)THEN
          WRITE(LISTA(M+2),116)IPM,MPM
  116     FORMAT   ('0 Page --- part: ',I2,' of ',I2,' ---')
        ENDIF
      ENDIF
      if(MMOD.EQ.8)then
        LISTA(M+3)=' '
      else
        LISTA(M+3)='< index select'
      endif
      LISTA(M+4)  ='? help'
      LISTA(M+5)  ='- exit this menu'

C Now display the menu (depending on the width of the strings).
      IF(MMOD.EQ.8)THEN
        impx=0
        impy=0
        iw=ISTRW+4
        call VWMENU(SEL,LISTA,MVERT,impx,impy,iw,irpx,irpy,ivert)
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
          CALL PHELPD('user pick string',NHELP,'-',0,0,IER)
        ELSEIF(NHELP.EQ.60)then
          CALL PHELPD('user pick string',NHELP,'-',0,0,IER)
        ELSEIF(NHELP.EQ.0)then
          WRITE(H(1),'(A,A)')'No help available for ',ERMSG
          CALL PHELPD('user pick string',2,'-',0,0,IER)
        ENDIF
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

C Passed parameters.
      CHARACTER*(*) prompt,title,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT
      CHARACTER*(*) HOPT,IOPT,JOPT,KOPT,LOPT
      integer index   ! current index
      integer idindex ! default index
      integer nhelp   ! number of help lines to display

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      LOGICAL SELECT

      DIMENSION VERT(16)
      CHARACTER VERT*48,KEY*1,prompt2*36
      integer MVERT,IVERT  ! max items and current menu item

#ifdef OSI
      integer impx,impy,iw
#else
      integer*8 impx,impy,iw
#endif

C Generate default help text, clear local menu string and remember
C the incomming index.
      call helpwithblank(title,nhelp,newnbhelp,ier)
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
      if(idindex.gt.0)then
        CALL EMKEY(idindex,KEY,IER)
        write(VERT(nopt+2),'(3a)') '* default is option `',KEY,'`'
        if(ISTRW.lt.17)write(VERT(nopt+2),'(3a)')
     &                              '* default is option `',KEY,'`'
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
        impx=0
        impy=0
        iw=ISTRW+2
        call VWMENU(title,VERT,MVERT,impx,impy,iw,irpx,irpy,ivert)
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
        CALL PHELPD(title,newnbhelp,'-',0,0,IER)
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

      END

C ******************** ASKOK
C ASKOK Generic choice facility returning logical variable
C from a yes no (with default indicated) prompt (supports help).
C In the case of a graphic menu the messages will appear in a
C dialogue box at the bottom of the graphic window.
C If NHELP=-1 there is no default option.
      SUBROUTINE ASKOK(MSG1,MSG2,OK,DOK,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      CHARACTER*(*) MSG1,MSG2
      CHARACTER ANS*2,MSG3*124,H*72,outs*124
      logical ok,dok,DEFLT
      integer IWI  ! for radio button

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
            WRITE(MSG3,'(2a)')MSG2(1:LN),' [Y]es (default) or [N]o ?'
          else
            WRITE(MSG3,'(2a)')MSG2(1:LN),' [Y]es or [N]o (default) ?'
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
        READ(IUIN,'(A2)',IOSTAT=IOS,ERR=1)ANS
        IF(ANS(1:1).EQ.'Y'.OR.ANS(1:1).EQ.'y')THEN
          OK=.TRUE.
        ELSEIF(ANS(1:1).EQ.'N'.OR.ANS(1:1).EQ.'n')THEN
          OK=.FALSE.
        ELSEIF(DEFLT.and.(ANS(1:1).EQ.'D'.OR.ANS(1:1).EQ.'d'))THEN
          OK=DOK
        ELSE
          CALL USRMSG(' ','You must make a choice!','-')
          GOTO 21
        ENDIF
      ELSEIF(MMOD.EQ.8)THEN

C Querry yes or no or default.
        IWI=1
        if (DEFLT) then
          if(DOK)then
            call EASKATOG(MSG1,MSG2,'yes (default)','no',' ',' ',' ',
     &      ' ',' ',IWI,NHL)
          else
            call EASKATOG(MSG1,MSG2,'yes','no (default)',' ',' ',' ',
     &      ' ',' ',IWI,NHL)
          endif
        else
          call EASKATOG(MSG1,MSG2,'yes','no',' ',' ',' ',
     &      ' ',' ',IWI,NHL)
        endif
        IF(IWI.eq.1)THEN
          OK=.TRUE.
        ELSEIF(IWI.eq.2)THEN
          OK=.FALSE.
        ELSEIF(DEFLT)THEN
          if(DOK)then
            OK=DOK
          elseif(.NOT.DOK)then
            OK=.FALSE.
          endif
        ENDIF
        call usrmsg('  ','  ','-')
      ENDIF
      RETURN

   1  if(IOS.eq.2)then
        write(outs,*) 
     &  'ASKOK: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'ASKOK: error composing prompt.'
        call edisp(iuout,outs)
      endif
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

C Parameters passed.
      CHARACTER*(*) MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT
      integer IWHICH ! selection passed back
      integer NHELP  ! number of help lines to display

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER ANS*2,MSG3*248,H*72

C Generate custom help text and copy NHELP to local variable.
      NHL=NHELP
      if(NHL.eq.0)then
        H(1)='Select an option.'
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
        call lusrmsg(msg1,msg3,'?')
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
          call usrmsg('The current answer is blank!',
     &                'Please re-enter.','W')
          goto 21
        ELSE
          CALL USRMSG(' ','You must make a choice!','-')
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

      END

C ************* EASKPER
C EASKPER: Provides legacy interface to the more general eAskPerGen below.
      SUBROUTINE eAskPer(PROMP1,IBDOY,IEDOY,IFDAY,IER)
      CHARACTER*(*) PROMP1
      integer ibdoy,iedoy,ifday,iBYr,iEYr,ier

      logical bMY_enabled
      parameter ( bMY_enabled = .false. )

C Note: initialize iBYr and iEYr even though the are not 
C returned. They're not referenced when multiyear simulations are disabled
      iBYr=2000
      iEYr=2000
      call eAskPerGen(PROMP1,IBDOY,IEDOY,
     &                 iBYr,iEYr,IFDAY,bMY_enabled,IER)

      return
      end


C ************* EAskPerYear
C EASKPER: Provides multi-year capable interface to the more general
C eAskPerGen below.
      SUBROUTINE eAskPerYear(PROMP1,ibdoy,iedoy,iBYr,iEYr,ifday,ier)
      CHARACTER*(*) PROMP1
      integer ibdoy,iedoy,iBYr,iEYr,ifday,ier

      logical bMY_enabled
      parameter ( bMY_enabled = .true. )

C initial values of iBYr iEYr are not need for single-year simulation
      call eAskPerGen(PROMP1,ibdoy,iedoy,iBYr,iEYr,ifday,bMY_enabled,
     &                IER)

      return
      end


C ************* eAskPerGen
C eAskPerGen: Provides interface to specification of a multiyear period of days. It
C returns IBDOY and IEDOY based on the current settings of IFDAY.
C iBYr,iEYr are the begining year and ending year of the period.
C Promp1 gives the context of the request for a period.
      SUBROUTINE eAskPerGen(PROMP1,ibdoy,iedoy,iBYr,iEYr,ifday,
     &                      bMY_enabled,IER)
      integer ibdoy,iedoy,iBYr,iEYr,ifday,ier
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

        IF(ifday.EQ.0)THEN
C Day of year to be provided

          if ( .not. bMY_enabled ) then
C Single year version
            H(1)='Period  must be within a calendar year.'

C Get start & end day
            write(HOLD,'(I4,I4)')ibdoy,iedoy
            CALL EASKS(HOLD,PROMP1,' Start and end day of year?',16,
     &      ' 1  1 ','start & end day of year',IER,1)

            CALL EGETWI(HOLD,K,ibdoy,1,365,'F','start doy',IER)
            if ( ier .ne. 0 ) bDateOK = .false.

            CALL EGETWI(HOLD,K,iedoy,1,365,'F','end doy',IER)
            if ( ier .ne. 0 ) bDateOK = .false.


          else
C Multi-year version
            H(1)='Provide day-of-year and year for start'
            H(2)='and end dates.'

C Get start day and year
            write(HOLD,'(I4,I5)') ibdoy,iBYr
            CALL EASKS(HOLD,PROMP1,' Start day and year?',16,
     &      ' 1  2000 ','start day and year',IER,2)

            CALL EGETWI(HOLD,K,ibdoy,1,365,'F','start doy',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

            CALL EGETWI(HOLD,K,iBYr,1900,2100,'F','start year',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

C Reset string index K
            K=0

C Get end day and year
            write(HOLD,'(I4,I5)') iedoy,iEYr
            CALL EASKS(HOLD,PROMP1,' End day and year?',16,
     &      ' 365 2000 ','end day and year',IER,2)

            CALL EGETWI(HOLD,K,iedoy,1,365,'F','end doy',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

            CALL EGETWI(HOLD,K,iEYr,1900,2100,'F','end year',IER)
            if  ( ier .ne. 0 ) BDateok = .false.

          endif !<- matches if ( .not. bMY_enabled )...

        ELSEIF ( ifday .eq. 1 .or. ifday .eq. 2 ) THEN

C Day & month or day, month & year to be provided
C Start date: Convert to dd mm format:

          CALL EDAYR(ibdoy,IBD,IBM)

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

C Check range, then convert to ibdoy.
          if ( bDateOK ) CALL EDAYCH(IBD,IBM,ier)

          if ( ier .eq. 1 ) bDateOK = .false.

          if ( bDateOK ) CALL EDAY(IBD,IBM,ibdoy)

C Only ask for end date if start-date successfully defined
          if ( bDateOK ) then

C End date, set at least equal to the beginning day.
            if ( .not. bMY_enabled .and. iedoy.le.ibdoy ) then
              iedoy = ibdoy + 1
            elseif ( bMY_enabled ) then

C Convert end-date into EDAYR friendly format and
C check that end date is not before start-date.
              iedoy = iedoy - ( iEYr - iBYr ) * 365
              if ( ( iEYr .eq. iBYr .and. iedoy .le. ibdoy ) .or.
     &             ( iEYr .lt. iBYr ) ) then
                iedoy=ibdoy+1
                iEyr=iBYr
                if (iedoy .gt. 365 ) then
                  iedoy = 1
                  iEYr = iEyr + 1
                endif
              endif
            endif ! <- matches ' elseif (bMY_enabled)...

C Convert end-day to day-month format
            CALL EDAYR(iedoy,IED,IEM)

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

C Check range, then convert to iedoy.
            if ( bDateOK )  CALL EDAYCH(IED,IEM, ier )

            if ( ier .ne. 0 ) bDateOK = .false.

            if ( bDateOK ) CALL EDAY(IED,IEM,iedoy)

C For multi-year simulations, convert iedoy:
            iedoy = iedoy + ( iEYr - iBYr ) * 365

          endif
        endif
      enddo

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
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      common/gfont/ifs,itfs,imfs
      CHARACTER*(*) TITLE
      CHARACTER HEAD*255

      CHARACTER*5 MM
      integer ifont   ! to pass to winfnt
      integer immod   ! to pass to jwinint

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
        immod=8
        call jwinint(immod,HEAD)
        ifont=1
        call winfnt(ifont)

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

  500 if(IOS.eq.2)then
        call edisp(iout,
     &    'Permission error reading terminal type. Try again.')
      else
        call edisp(iout,'Error reading terminal type. Try again.')
      endif
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
      integer irpx,irpy

C To conform to Fortran -> C passing conventions on 32bit and 64bit
#ifdef OSI
      integer impx,impy
#else
      integer*8 impx,impy
#endif

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
        irpx=0
        irpy=0
        impx=0
        impy=0
        CALL EWMENU(NAME,ITEMS,NITMS,impx,impy,irpx,irpy,INO)
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
   1  if(IOS.eq.2)then
        call edisp(IUOUT,'emenu: permission error composing prompt.')
      else
        call edisp(IUOUT,'emenu: error composing prompt.')
      endif
      return
   2  if(IOS.eq.2)then
        call edisp(IUOUT,'emenu: permission error composing item.')
      else
        call edisp(IUOUT,'emenu: error composing item.')
      endif
      return
      END


C++++++++++ EWMENU +++++++++++++++++++++++++++++++++++++++++++++
C EWMENU: Is the binding to C function for menu dialogue.  It
C allows the string widths to be variable widths. Uses width of
C items in call to evwmenu.
      SUBROUTINE EWMENU(name,items,nitms,impx,impy,irpx,irpy,ino)
      character*(*) name, items(*)
      integer irpx,irpy
      character itypes*40

C To conform to Fortran -> C passing conventions on 32bit and 64bit
C computers cast the parameters passed into ewmenu to local variables
C of either integer or integer*8 to match the long int variables in
C the C function evwmenu.  Do reverse cast after return from evwmenu.
#ifdef OSI
      integer iw,irpxe,irpye,inoe,ipflg,iuresp  ! for use with evwmenu
      integer impx,impy
#else
      integer*8 iw,irpxe,irpye,inoe,ipflg,iuresp  ! for use with evwmenu
      integer*8 impx,impy
#endif

C Append a null terminator to the title string if possible.
      iw=LEN(items(1))
      write(itypes,9)
   9  format(40('-'))
      call updmenu(items,itypes,nitms,iw)
      inoe=ino   ! assign to local passed parameters
      irpxe=irpx
      irpye=irpy
      call evwmenu (name,impx,impy,iw,irpxe,irpye,inoe,ipflg,iuresp)
#ifdef OSI
      ino=inoe  !  cast back from returned parameter
      irpx=irpxe
      irpy=irpye
#else
      ino=int(inoe)  !  cast back from returned parameter to integer
      irpx=int(irpxe)
      irpy=int(irpye)
#endif
      if (ino.lt.0) then

C Overload INO with click position and button.  A nine digit number is
C used the first digit is the mouse button and the next 4 represent the
C x pixel and the last 4 the y pixel.
        ino=-100000000*iuresp-10000*irpx-irpy
      endif
C Debug...
C      write(6,'(a,7i10)') 'EWMENU (fortran) ',iw,irpx,irpy,ino,inoe,
C     &  ipflg,iuresp
      return
      end

C++++++++++ VWMENU +++++++++++++++++++++++++++++++++++++++++++++
C VWMENU: Is the binding to C function for menu dialogue.  It
C allows the string widths to be variable widths.
      SUBROUTINE VWMENU(name,items,nitms,impx,impy,iw,irpx,irpy,ino)
      character*(*) name, items(*)
      character itypes*40
      integer irpx,irpy

C To conform to Fortran -> C passing conventions on 32bit and 64bit
C computers cast the parameters passed into ewmenu to local variables
C of either integer or integer*8 to match the long int variables in
C the C function evwmenu.  Do reverse cast after return from evwmenu.
#ifdef OSI
      integer impxe,impye,iwe,irpxe,irpye,inoe,ipflg,iuresp  ! for use with evwmenu
      integer impx,impy,iw
#else
      integer*8 impxe,impye,iwe,irpxe,irpye,inoe,ipflg,iuresp  ! for use with evwmenu
      integer*8 impx,impy,iw
#endif
      iwe=iw    ! assign to local passed parameter
      write(itypes,9)
   9  format(40('-'))
      call updmenu(items,itypes,nitms,iwe)
      inoe=ino   ! assign to local passed parameters
      impxe=impx   
      impye=impy   
      irpxe=irpx
      irpye=irpy
      call evwmenu (name,impxe,impye,iwe,irpxe,irpye,inoe,ipflg,iuresp)
#ifdef OSI
      ino=inoe  !  cast back from returned parameter
      irpx=irpxe
      irpy=irpye
#else
      ino=int(inoe)  !  cast back from returned parameter to integer
      irpx=int(irpxe)
      irpy=int(irpye)
#endif

C Debug
C      write(6,*) 'VWMENU (fortran) ',iw,irpx,irpy,ino,ipflg,iuresp
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

   1  if(IOS.eq.2)then
        call edisp(IUOUT,'USRMSG: permission error composing prompt.')
      else
        call edisp(IUOUT,'USRMSG: error composing prompt.')
      endif
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

   1  if(IOS.eq.2)then
        call edisp(IUOUT,'LUSRMSG: permission error composing prompt.')
      else
        call edisp(IUOUT,'LUSRMSG: error composing prompt.')
      endif
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
C The maximum width of text which can be accommodated to a graphic
C context is 124 characters.
      SUBROUTINE EDISP(ITRU,MSG)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/textbuf/dispbuf(360)
      common/textbufl/indexbuf,lnbuf(360)
      CHARACTER*(*) MSG
      CHARACTER WWMSG*124,dispbuf*124
      logical unixok

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
   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      if(IOS.eq.2)then
        if(lnm.le.1)then
          write(6,*) 'edisp: permission error writing blank/null '
        else
          write(6,*) 'edisp: permission error writing text: ',MSG
        endif
      else
        if(lnm.le.1)then
          write(6,*) 'edisp: error writing blank/null '
        else
          write(6,*) 'edisp: error writing text: ',MSG
        endif
      endif
      return
   2  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      if(IOS.eq.2)then
        write(6,*) 'edisp: permission error writing text: ',WWMSG
      else
        write(6,*) 'edisp: error writing text: ',WWMSG
      endif
      return
      END


C ****** write current text buffer to an appropraite file
      subroutine proftxdump()
#include "building.h"
#include "model.h"

C Path to model and command line file (if any).
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/rpath/path
      common/pophelp/h(60)
      common/C21/IFCFG,cfgroot,LCFGF
      common/textbuf/dispbuf(360)
      common/textbufl/indexbuf,lnbuf(360)

      DIMENSION sstr(1),list(4),iswidth(1),listact(4),dh(60)
      character path*72,h*72,tfile*60,uname*24,fs*1
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

C Debug
C      write(6,*)listact,'  ',ino
      if(ino.eq.-1)then

C If no action return.
        return
      elseif(ino.eq.(nlist+1))then

C If signaled ok then attempt to write out file.
C        write(6,*)sstr(1)
        tfile=sstr(1)
        ltf=max(1,LNBLNK(tfile))
        iuf=ifil+1
        there=.false.
        INQUIRE(FILE=tfile(1:ltf),EXIST=there)
        if(there)then
          close(iuf)
          open(iuf,file=tfile(1:ltf),position='APPEND',
     &      status='UNKNOWN',err=1)
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

   1  if(IOS.eq.2)then
        call usrmsg('Permissions open/write dump file...',tfile,'W')
      else
        call usrmsg('Could not open/write dump file...',tfile,'W')
      endif
      return
   2  if(IOS.eq.2)then
        call usrmsg('Permissions for append to dump file...',tfile,'W')
      else
        call usrmsg('Could not append to dump file...',tfile,'W')
      endif
      return

      end

C ****** invoke graphics capture proforma
      subroutine profgrdump()
#include "building.h"
#include "model.h"

C Path to model and command line file (if any).
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      common/rpath/path
      common/pophelp/h(60)
      common/C21/IFCFG,cfgroot,LCFGF

C Graphic capture, window dump: label, command.
      common/gprint/gprlbl,gprcmd
      common/gxwd/gxwdlbl,gxwdcmd

      DIMENSION sstr(1),list(4),iswidth(1),listact(4),dh(60)
      character path*72,h*72,tfile*60,uname*24,fs*1
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

C Debug.
C      write(6,*)listact,'  ',ino
      if(ino.eq.-1)then

C If no action return.
        return
      elseif(ino.eq.(nlist+1))then

C If signaled ok then attempt to write out file.
C        write(6,*)sstr(1)
        tfile=sstr(1)
        ltf=max(1,lnblnk(tfile))
        if(listact(2).eq.1)then
          LN=max(1,lnblnk(gprcmd))
          write(gcmd,'(3a)') gprcmd(1:LN),' ',tfile(1:ltf)
        elseif(listact(3).eq.1)then
          LN=max(1,lnblnk(gxwdcmd))
          write(gcmd,'(3a)') gxwdcmd(1:LN),' ',tfile(1:ltf)
        endif
C Debug
C        write(6,*)gcmd
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
      character tut_path*72,ltut*72,IA*3,fs*1
      logical extra,XST
      logical unixok
      integer ltf    ! last actual character in the string.
      integer ln     ! last actual character in instpath (of espinstalldir.h)

C The following include statment is placed after the common block
C definitions to avoid compile errors.
#include "espinstalldir.h"

C dh is duplicate help list, lh is the list (page) to be displayed.
C exh is 'show me more' text array read in from external file.
      extra = .false.
      ln=lnblnk(instpath)

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      if(OTHER(1:1).ne.'-')then
        write(tut_path,'(4a)')instpath(1:ln),fs,'tutorial',fs
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
        else
          extra = .false.
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

   1  if(IOS.eq.2)then
        write(outs,*) 'EHELPD: permission error writing help message'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EHELPD: error writing help message'
        call edisp(iuout,outs)
      endif
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


C ************************ dupphelp
C dupphelp copies current pop-uphelp common into a string array so that
C C code does not destroy it.
      SUBROUTINE dupphelp(dh)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      dimension dh(60)
      CHARACTER H*72,dh*72,outs*124

      DO 50 I=1,60
        last=max(LNBLNK(h(I)),1)
        WRITE(dh(i),'(A)',iostat=ios,err=1)h(I)(1:last)
   50 CONTINUE
      RETURN
   1  write(outs,*) 'dupphelp: error copying help line. '
      call edisp(iuout,outs)
      return
      END

C ********************* PHELPW
C PHELPW returns the width IWH of the longest text string in the common
C pophelp where IL is the number of lines of help under consideration.
      SUBROUTINE PHELPW(IL,IHW,IER)
      common/pophelp/h(60)
      CHARACTER h*72
      integer ill   ! local loop

      IER=0
      IHW=0
      ill=il                ! cast to local loop
      IF(ILL.GT.60) ILL=60  ! protect against array error
      IF(ILL.GT.0)THEN
        DO 10 I=1,ILL
          IX=max(1,LNBLNK(h(I)))
          IF(IX.GT.IHW)IHW=IX
  10    CONTINUE
      ENDIF
      RETURN
      END


C ************* EVSET
C EVSET provides setup environment for wire frame view w/in prj.
C IER=0 OK, IER=1 problem. If izgfoc is not 0
C then it represents the zone which is currently being edited.

      SUBROUTINE EVSET(act,IER)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"

      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/C1/NCOMP,NCON
      common/rpath/path
      common/appw/iappw,iappx,iappy
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/PREC8/SLAT,SLON

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
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK
      logical MODIFY,MODLEN,MODBND
      LOGICAL OPTKOK,CLOSE,OK,dok,found,concat,redraw
      DIMENSION ITEMS(23),IVALS(MCOM),FALT(7),IVAL(7)
      CHARACTER ITEMS*32,H*72,LFIL*72,outs*124
      CHARACTER FALT*37,MTHNAM*3,ETEXT*60
      CHARACTER DESCRH*5,DESCRD*5,path*72
      character doit*248,tmode*8,longtfile*144, hold*24,act*1
      CHARACTER head*19

      dimension mthnam(12),iavdec(12)
      data iavdec/17,15,16,15,15,11,17,16,16,16,15,11/
      data mthnam/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/
      integer IW  ! for radio buttons
      integer MVERT,IVERT  ! max items and current menu item

#ifdef OSI
      integer ivt2,ivt3,ivt4  ! for use with viewtext
      integer ICPMOD          ! passed from tchild
      integer iappr,iappt,iappb ! for use with findrtb
      integer iapprp,iapptp,iappbp ! for local offsets
#else
      integer*8 ivt2,ivt3,ivt4  ! for use with viewtext
      integer*8 ICPMOD          ! passed from tchild
      integer*8 iappr,iappt,iappb ! for use with findrtb
      integer*8 iapprp,iapptp,iappbp ! for local offsets
#endif

C If not in graphic mode return.
      if(MMOD.lt.8)return

C Initial menu entry setup with one line header, 3 control lines.
      redraw=.false.
      ITPPSW=0

C Menu loop.
   92 IER=0
      IVERT=-3

      WRITE(ITEMS(1),'(A,3F6.1)')'a eye point:',(EYEM(J),J=1,3)
      WRITE(ITEMS(2),'(A,3F6.1)')'b view point:',(VIEWM(J),J=1,3)
      WRITE(ITEMS(3),'(A,F6.1)') 'c angle of view:',ANG
      if(ITPPSW.eq.0)then
        ITEMS(4) ='d display >> perspective     '
      elseif(ITPPSW.eq.1)then
        ITEMS(4) ='d display >> plan view       '
      elseif(ITPPSW.eq.2)then
        ITEMS(4) ='d display >> south elevation '
      elseif(ITPPSW.eq.3)then
        ITEMS(4) ='d display >> west elevation  '
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
        ITEMS(15)='m grid distance       optimum   '
      ELSE
        WRITE(ITEMS(15),23)GRDIS
  23    FORMAT('m grid distance:',F5.2,' (m)')
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

C If image info altered (view point) then update display.
      IF(MODIFY)THEN
        if(ITPPSW.eq.0)then
          CALL INLNST(1)
          CALL ADJVIEW(IER)
          IF(ier.NE.0)RETURN
          MODLEN=.FALSE.
          MODBND=.FALSE.
          MODIFY=.FALSE.
        elseif(ITPPSW.eq.1)then
          if(redraw)call PLELEV('P')
        elseif(ITPPSW.eq.2)then
          if(redraw)call PLELEV('S')
        elseif(ITPPSW.eq.3)then
          if(redraw)call PLELEV('E')
        endif
      ENDIF

C Now display the menu. If in response to image control button
C then shift its position slighty (taking into account the current
C width of the control menu.
      if(act.eq.'-')then
        head='Viewing Environment'
        CALL EMENU(head,ITEMS,MVERT,IVERT)
      elseif(act.eq.'B')then
        call findrtb(iappr,iappt,iappb)
        head='Image Control'
        if(menuchw.lt.10)then
          iapprp=iappr-160
          iapptp=32
          iappbp=iappb-50
          call vwmenu (head,ITEMS,MVERT,iapprp,
     &      iappbp,iapptp,irpx,irpy,IVERT)
        elseif(menuchw.ge.10.and.menuchw.lt.20)then
          iapprp=iappr-120
          iapptp=32
          iappbp=iappb-50
          call vwmenu (head,ITEMS,MVERT,iapprp,
     &      iappbp,iapptp,irpx,irpy,IVERT)
        elseif(menuchw.ge.20.and.menuchw.lt.30)then
          iapprp=iappr-60
          iapptp=32
          iappbp=iappb-50
          call vwmenu (head,ITEMS,MVERT,iapprp,
     &      iappbp,iapptp,irpx,irpy,IVERT)
        elseif(menuchw.ge.30.and.menuchw.lt.35)then
          iapprp=iappr-20
          iapptp=32
          iappbp=iappb-50
          call vwmenu (head,ITEMS,MVERT,iapprp,
     &      iappbp,iapptp,irpx,irpy,IVERT)
        elseif(menuchw.ge.35)then
          iapprp=iappr-10
          iapptp=32
          iappbp=iappb-50
          call vwmenu (head,ITEMS,MVERT,iapprp,
     &      iappbp,iapptp,irpx,irpy,IVERT)
        endif
      endif
      IF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.1)THEN
        H(1)='The eye point is within the site coordinate system.'
  43    WRITE(HOLD,'(1x,3f7.1)')EYEM(1),EYEM(2),EYEM(3)
        CALL EASKS(HOLD,' ','Eye point X Y Z?',
     &     24,' -100. -100. 100. ','eye point coord',IER,1)
        K=0
        CALL EGETWR(HOLD,K,EYEM(1),-999.,999.,'W','X cord',IER)
        CALL EGETWR(HOLD,K,EYEM(2),-999.,999.,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,EYEM(3),0.,999.,'W','eye Z cord',IER)
        if(ier.ne.0)goto 43
        MODLEN=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.2)THEN

C Present viewpoint coords for editing then parse data from HOLD.
        H(1)='The view point is a point within the site coordinate'
        H(2)='system. The default is the centre of gravity of the'
        H(3)='model.'
  42    WRITE(HOLD,'(1x,3f7.1)')VIEWM(1),VIEWM(2),VIEWM(3)
        CALL EASKS(HOLD,' ','View point X Y Z?',
     &           24,' 2. 2. 1. ','viewpoint coord',IER,3)
        K=0
        CALL EGETWR(HOLD,K,VIEWM(1),-999.,999.,'W','X cord',IER)
        CALL EGETWR(HOLD,K,VIEWM(2),-999.,999.,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,VIEWM(3),0.,999.,'W','view Z cord',IER)
        if(ier.ne.0)goto 42
        MODLEN=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.3)THEN
        H(1)='The angle of view is between 1 and 89 degrees.'
        CALL EASKR(ANG,' ','Angle of view?',
     &             1.0,'W',89.0,'W',40.,'angle ofview',IER,1)
        HANG=ANG/2.0
        MODLEN=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.4)THEN
        H(1)='In plan view you can change the viewing parameters;'
        H(2)='the elevations are primarily for checking.'
        IW=1
        CALL EASKABCD(' ','View options:','perspective',
     &    'plan','south elevation','west elevation',IW,2)
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
          H(2)='of the model can be studied. Current filters are'
          H(3)='listed.'
          FALT(1)='all surfaces and shading obstructions'
          FALT(2)='all surfaces (no obstructions)       '
          FALT(3)='exterior surfaces only               '
          FALT(4)='partitions & interior surfaces only  '
          FALT(5)='surf. connected to ground or constant'
          FALT(6)='all surfaces, obstructions & ground  '
          FALT(7)='ground topology only                 '
          IV=1
          CALL EPICKS(IV,IVAL,' ','Image filter:',
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
        h(1)='match the given filter. '
        IW=1
        CALL EASKATOG(' ','Highlight options:','normal','composition',
     &    'opaque:transp.','partial attrib.','cancel ?',' ',' ',IW,2)
        ITHLS=IW-1
        if(ITHLS.eq.0)then
          MODIFY=.true.
        elseif(ITHLS.eq.1)then
          CALL EPKMLC(ISEL,'Select a construction to hilight.',' ',IER)
          ITHLZ=ISEL
          MODIFY=.TRUE.
        elseif(ITHLS.eq.2)then
          CALL EASKABC(' ','Opacity options:','hilight opaque',
     &    'hilight transparent','cancel ?',IW,2)
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
        H(2)='altered for clarity. This is especially useful in'
        H(3)='for large models. If set to 0 then an optimum value'
        H(4)='will be used.'
        CALL EASKR(GRDIS,' ','Grid distance?',
     &             0.0,'F',10.0,'W',0.,'grid distanc e',IER,4)
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.16)THEN
        ITSNR=ITSNR+1
        IF(ITSNR.GT.1)ITSNR=0
        MODIFY=.TRUE.
      ELSEIF(IVERT.EQ.18)THEN

C Select zones to include, if one zone is the principal focus then
C make sure it is included in the list.
        IF(CFGOK)THEN
          H(1)='Pick one, several or all zones for inclusion.'
          H(2)='in image.'
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ','Which zones to include:',
     &      12,NCOMP,zname,'zone list',IER,2)
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
          CALL USRMSG('Model not yet defined!',
     &    'Please select option `a` of model definition menu.','W')
          RETURN
        ENDIF
      ELSEIF(IVERT.EQ.19.AND.INPIC.GE.1)THEN

C Refresh the image.
        MODLEN=.TRUE.
        MODBND=.TRUE.
        MODIFY=.TRUE.
        do 444,ix=1,ncomp
          iZBFLG(ix)=0
  444   continue
        CALL ADJVIEW(IER)
        IF(ier.NE.0)RETURN
        MODLEN=.FALSE.
        MODBND=.FALSE.
        MODIFY=.FALSE.
      ELSEIF(IVERT.EQ.20)THEN

C Construct a perspective image file. The trace level defines whether
C information about the zones is displayed or not. Because users
C who may be working in remote folders may wish to save locally,
C trap this condition.
        IUO=IFIL+2
        iw=0
        if(path.ne.'./'.and.path.ne.' ')then
          write(outs,'(A,A)') 'The current path is: ',path
          call edisp(iuout,outs)
          CALL EASKAB('You are working in a remote folder.',
     &    'Options:','use remote folder','use local folder',IW,0)
         endif

C Get file name.
        H(1)='A viewer format file may be used with the 3rd party'
        H(2)='wire-frame & hidden line program VIEWER.'
        CALL EASKS(LFIL,' ','Perspective image file? ',
     &              72,' ','perspective input file',IER,2)
        if(iw.eq.1)then
          CALL EFOPSEQ(IUO,LFIL,4,IER)
          write(currentfile,'(a)') LFIL(1:lnblnk(LFIL))
        else
          CALL ERPFREE(IUO,ISTAT)
          call FPOPEN(IUO,ISTAT,1,3,LFIL)
          write(currentfile,'(a)') LFIL(1:lnblnk(LFIL))
        endif
        CALL EMKVIEW(IUO,CFGOK,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(' ',
     &          'Problem creating perspective image file!','W')
          goto 92
        ENDIF
        dok=.true.
        h(1)='Be sure to tidy up any *.vew files that might'
        h(2)='have been automatically created.'
        CALL ASKOK(' ','Display perspective views?',OK,dok,2)
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
C          call usrmsg(' ','Starting perspective viewer.','-')
          call runit(doit,tmode)
        endif
      ELSEIF(IVERT.EQ.21)THEN

C Draw the zones and any obstructions based on sun position
C at user specified time (offer 11 am on day of average solar
C declination in June).
        NTS=1
        IMO=6
        IDO=iavdec(IMO)
        STIME=11.

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
          CALL ADJVIEW(IER)

C Generate a heading for the view.
          CALL EDTIME(STIME,DESCRH,DESCRD,TIMER)
          WRITE(ETEXT,'(A,I2,1X,A3,A,A5,A,F6.1,A,F5.1)')
     &      ' View: ',IDO,MTHNAM(IMO),' @ ',DESCRH,
     &      ' Solar azimuth',SAZI,', elevation',SALT
          ivt2=2
          ivt3=1
          ivt4=IFS
          CALL viewtext(ETEXT,ivt2,ivt3,ivt4)
          CALL EASKAB(' ','New view point:',
     &        'next hour','exit',IW,0)
          if(IW.eq.1)then
            STIME=STIME+((60.0/FLOAT(NTS))/60.0)
            goto 68
          else
            ITSNM=0
            goto 92
          endif
        else
          CALL EASKAB('View point below horizon!','Options:',
     &        'another time','exit',IW,0)
          if(IW.eq.1)goto 67
          ITSNM=0
        endif
      ELSEIF(IVERT.EQ.22)THEN

C Produce help text for the  menu.
        H(1)='The Image Control menu allows adjustment of the control'
        H(2)='parameters relating to the model geometry begin. In most'
        H(3)='cases the image will be refreshed as editing takes place.'
        H(4)='In the case of inserted vertices that have not yet been'
        H(5)='associated with surfaces, these will be indeicated as'
        H(6)='circles.'
        CALL PHELPD('3dv menu',6,'-',0,0,IER)
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-2
      GOTO 92

      END

C ******** startbuffer
C Wrapper around call to win3dclr in esru_x.c to match
C the functionality of startbuffer in the GTK version.
       subroutine startbuffer()
       call win3dclr
       return
       end

C popupimage() display image with documentation (dummy of GTK version)
      subroutine popupimage(head,topic,act,longtfile)
      character*(*) head,topic,act,longtfile
      return
      end

