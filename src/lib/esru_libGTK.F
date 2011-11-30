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

C  EVSET:   provides dummy setup environment for wire frame view.

C Dummy subroutines and functions matching calling parameters used
C in esru_x.c so that #ifdef statements are not required for GTK
C compile of ESP-r. This same code is found in esru_fc.f which is
C used in the text-only compile.
C       opencpw place copyright button on screen
C       opensetup place setup button on screen
C       updcapt() notify level for capture buttons
C       updazi() notify level for azimuth button
C       feedbox() open feedback background box
C       opengdisp opens a scrolling text display area.


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
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*10,STR1*10,STR2*10
      CHARACTER ask*16,OUTSTR*124,DSTR*124,outs*124
      character prompt*144
      LOGICAL OK,DOK,unixok
      integer ivalold   ! to remember passed value in case of cancel

      IER=0

C If IVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.IVAL.LT.MINV)IVAL=IDEFLT
      if(MAXACT.EQ.'F'.and.IVAL.GT.MAXV)IVAL=IDEFLT

C Clear string buffers
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
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)

        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif

C Debug.
C        write(6,*) '< ask prompt ',PROMPT(1:lnblnk(PROMPT))
C        write(6,*) '< ask string ',ask(1:lnblnk(ask))
        CALL askdialog(PROMPT,ask,IFLG)  ! pass it the int as a string

C Debug.
C        write(6,*) 'ask VAL:',ask,' iflg ',IFLG

        IF(iflg.EQ.-3)THEN

C Cancel button was pressed, reset ival as ivalold and mark as cancel. 
          IVAL=ivalold
          CALL USRMSG('User requested a cancel for dialog',PROMPT,'-')
          ier=-3
          RETURN
        ELSEIF(iflg.EQ.-2)THEN

C A default answer detected, set to default value.
          call edisp(iuout,DSTR)
          IVAL=IDEFLT
          RETURN
        ELSE

C A valid number or interaction.
          call c2fstr(ask,WORD)

C Debug.
C          write(6,*) 'ask WORD: ',WORD
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
          call isunix(unixok)
          if(unixok)then
            WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55       FORMAT('The existing value (',a,') will be used.')
          endif
          RETURN
        ENDIF
      ENDIF

C Decode the string (WORD) into an integer.
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
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*16,STR2*16
      CHARACTER OUTSTR*124,DSTR*124,outs*124
      character ask*16,prompt*144
      LOGICAL OK,close,DOK,unixok

C If RVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.RVAL.LT.RMIN)RVAL=DEFLT
      if(MAXACT.EQ.'F'.and.RVAL.GT.RMAX)RVAL=DEFLT

C Check that value is not trivially close to zero (i.e. not
C instantiated on the Fortran side before the call).
   20 WORD=' '
      CALL ECLOSE(RVAL,0.00,0.000001,CLOSE)
      if(close)then
        RVAL=0.0
        ask='   0.00   '
      else
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

C If in graphic mode use graphic facilities to edit the number.
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
        if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif

C Debug.
C        write(6,*) '< askr prompt ',PROMPT(1:lnblnk(PROMPT))
C        write(6,*) '< askr string ',ask(1:lnblnk(ask))
        CALL askdialog(PROMPT,ask,IFLG)

C Debug.
C        write(6,*) 'askr VAL:',ask,' iflg ',IFLG

        IF(iflg.EQ.-3)THEN

C A cancel answer detected, set ier to -3 and return.
          CALL USRMSG('User requested a cancel for dialog',PROMPT,'-')
          ier=-3
          RETURN
        ELSEIF(iflg.EQ.-2)THEN

C A default answer detected, set to default value.
          call edisp(iuout,DSTR)
          RVAL=DEFLT
          call usrmsg('  ','  ','-')
          RETURN
        ELSE

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
          call c2fstr(ask,WORD)
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
          CALL REL16STR(RVAL,STR2,IW2,IER)
          call isunix(unixok)
          if(unixok)then
            WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55       FORMAT('The existing value (',a,') will be used.')
          endif
          RETURN
        ENDIF
      ENDIF

C Decode the string into a real.
      read(WORD,*,ERR=89)rvalt
      CALL REL16STR(RVALT,STR1,IW1,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ','Did not understand, try again..','-')
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
C to identify the value.
C IER is the error state, if 0 then OK, if -3 cancel button pressed.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.

      SUBROUTINE EASKE(RVAL,PROMP1,PROMP2,RMIN,MINACT,
     &                 RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*10,STR2*10
      CHARACTER ask*16,OUTSTR*124,DSTR*124,outs*124
      character prompt*144
      LOGICAL OK,close,DOK,unixok

      IER=0

C If RVAL is outwith range for failure then set to default.
      if(MINACT.EQ.'F'.and.RVAL.LT.RMIN)RVAL=DEFLT
      if(MAXACT.EQ.'F'.and.RVAL.GT.RMAX)RVAL=DEFLT

C Check that value is not trivially close to zero (ie not instantiated
C on the fortran side before the call).
      CALL ECLOSE(RVAL,0.00,0.000001,CLOSE)
      if(close)RVAL=0.00
   20 WORD=' '
      WRITE(ask,'(1PE14.6)')RVAL

C Generate help and default strings.
      CALL EXPSTR(DEFLT,STR2,IW2,IER)
      WRITE(DSTR,27,iostat=ios,err=1)STR2(1:IW2)
   27 FORMAT(' The default value (',a,') will be used.')

      CALL EXPSTR(DEFLT,STR1,IW1,IER)
      CALL EXPSTR(RVAL,STR2,IW2,IER)
      call helpcurrentreal(deflt,rval,ermsg,nhelp,newnbhelp,ier)

C Begin with prompts.
      IF(MMOD.EQ.8)THEN

C If in graphic mode use graphic facilities to edit the number.
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
        if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif

C Debug.
C        write(6,*) '< aske prompt ',PROMPT(1:lnblnk(PROMPT))
C        write(6,*) '< aske string ',ask(1:lnblnk(ask))
        CALL askdialog(PROMPT,ask,IFLG)

C Debug.
C        write(6,*) 'aske VAL:',ask,' iflg ',IFLG

        IF(iflg.EQ.-3)THEN

C A cancel answer detected, set ier to -3 and return.
          CALL USRMSG('User requested a cancel for dialog',PROMPT,'-')
          ier=-3
          RETURN
        ELSEIF(iflg.EQ.-2)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,DSTR)
          RVAL=DEFLT
          call usrmsg('  ','  ','-')
          RETURN
        ELSE

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
          call c2fstr(ask,WORD)
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
          call isunix(unixok)
          if(unixok)then
            WRITE(IUOUT,55,iostat=ios,err=1)STR2(1:IW2)
   55       FORMAT(' The existing value (',a,') will be used.')
          endif
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
C  IER is the error state, if 0 then OK if -3 then user requested CANCEL.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.

      SUBROUTINE EASKF(FILEN,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER*(*) PROMP1,PROMP2,ERMSG,FILEN,DSTR
      CHARACTER WORD144*144,A*1,LASTS*144,OUTSTR*124,outs*124
      character prompt*144
      LOGICAL OK,DOK,unixok

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

C Generate custom help text.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        WORD144=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        lmmod=mmod
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif

C << There are potentially several things that need updating >>
C << to this call and to askf (within esp_file.c). The GTK browse >>
C << dialog does not include a help button! >>
        CALL askf(PROMPT,FILEN)

C Debug
C        write(6,*) 'askdialog FILEN:',FILEN

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(FILEN,WORD144)

C << there is no default signal returned from the askf call so
C << comment out this section of the code for now.
C        IF(IFLG.EQ.-2)THEN
C          LN=max(1,LNBLNK(DSTR))
C          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)' The default is ',
C     &      DSTR(1:LN),'.'
C          DOK=.true.
C          CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,NHL)
C          IF(OK)THEN
C            FILEN=DSTR(1:LN)
C            RETURN
C          ELSE
C            GOTO 20
C          ENDIF
C        ENDIF
        IF(WORD144(1:2).EQ.'  ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            call isunix(unixok)
            if(unixok) WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
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
        ELSEIF(WORD144(1:6).EQ.'CANCEL')THEN

C The call to askf will return the first 7 characters as CANCEL if
C the user pressed the cancel button. Set ier = -3 to signal a
C cancel request to the calling code.
          if(last.lt.144)then
            write(FILEN,'(a)',iostat=ios,err=3) LASTS(1:last)
          else
            write(FILEN,'(a)',iostat=ios,err=3) LASTS(1:144)
          endif
          call edisp(iuout,'User requested a cancel during dialog...')
          call edisp(iuout,prompt)
          ier=-3
          RETURN
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
C IER is the error state, if 0 then OK, if -3 then cancel button pressed.
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

      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,outs*124
      character prompt*144
      LOGICAL OK,DOK,unixok
      integer last    ! last blank (or one) in the string.

   20 WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)
      if (last.gt.6) then
        if(STRVAL(1:7).eq.'UNKNOWN'.or.STRVAL(1:7).eq.'unknown'.or.
     &     STRVAL(1:7).eq.'Unknown') STRVAL='  '
      endif

C Generate custom help text and use local var for NHELP.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C At this point get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8 use dialogue box.
      IF(MMOD.EQ.8)THEN
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        lmmod=mmod
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif

C Debug.
C        write(6,*) '< askdialog prompt ',PROMPT(1:lnblnk(PROMPT))
C        write(6,*) '< askdialog string ',STRVAL(1:lnblnk(STRVAL))

        CALL askdialog(PROMPT,STRVAL,IFLG)

C Debug.
C        write(6,*) 'askdialog STRVAL:',STRVAL,' iflg ',IFLG

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned. It is assumed that STRVAL
C passed into the subroutine will fit within a 92 character WORD buffer.
        call c2fstr(STRVAL,WORD)

        IF(iflg.EQ.-3)THEN

C A cancel answer detected, set ier to -3 and return.
          CALL USRMSG('User requested a cancel for dialog',PROMPT,'-')
          ier=-3
          RETURN
        ELSEIF(iflg.EQ.-2)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
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
        IF(WORD.EQ.' '.or.IFLG.eq.-1)THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS.NE.' ')THEN
            call isunix(unixok)
            if(unixok) WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
            STRVAL=LASTS(1:last)
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
            call isunix(unixok)
            if(unixok)then
              WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
   55         FORMAT(' The existing string (',a,') will be used.')
            endif
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
C EASKS248 edit a (248 char) string in a multi-line graphic box or via phrases.

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C IER is the error state, if 0 then OK, if -3 then cancel button pressed.
C NHELP is the number of help lines and H() is the array
C of text strings to be printed out if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
      SUBROUTINE EASKS248(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,
     &   NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      CHARACTER t72*72,t72a*72,t72b*72,t72c*72
      character outstr*248,word*248,prompt*144,LASTS*248,outs*124
      LOGICAL next,OK,DOK,unixok

   20 IER=0
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)
      if (last.gt.6) then
        if(STRVAL(1:7).eq.'UNKNOWN'.or.STRVAL(1:7).eq.'unknown'.or.
     &     STRVAL(1:7).eq.'Unknown') STRVAL='  '
      endif

C Generate custom help text and use local var for NHELP.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

      call edisp(iuout,'Current...')
      call edisp248(iuout,STRVAL,72)
      call edisp(iuout,' ')

      IF(MMOD.EQ.8)THEN

C For graphic version use the standard GTK widget as does easks.
        IFLG=0
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        lmmod=mmod
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif

C Debug.
C        write(6,*) '< 248 askdialog prompt ',PROMPT(1:lnblnk(PROMPT))
C        write(6,*) '< 248 askdialog string ',STRVAL(1:lnblnk(STRVAL))

        CALL askdialog248(PROMPT,STRVAL,IFLG)

C Debug.
C        write(6,*) 'askdialog STRVAL:',STRVAL,' iflg ',IFLG

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)
C Debug
C        write(6,*) word

        IF(iflg.EQ.-3)THEN

C A cancel answer detected, set ier to -3 and return.
          CALL USRMSG('User requested a cancel for dialog',PROMPT,'-')
          ier=-3
          RETURN
        ELSEIF(iflg.EQ.-2)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)' The default is ',
     &      DSTR(1:LN),'.'
          DOK=.true.
          CALL ASKOK(OUTSTR,' Is this ok?',OK,DOK,newnbhelp)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            RETURN
          ELSE
            GOTO 20
          ENDIF
        ENDIF
        IF(WORD.EQ.' '.or.IFLG.eq.-1)THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS.NE.' ')THEN
            call isunix(unixok)
            if(unixok)then
              WRITE(IUOUT,55,iostat=ios,err=1)LASTS(1:last)
   55         FORMAT(' The existing string (',a,') will be used.')
            endif
            STRVAL=LASTS(1:last)
            RETURN
          ELSE
            CALL USRMSG(' The current string is blank!',
     &                  ' Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:lnblnk(WORD))

C Debug.
C        write(6,*) 'eask248 returning ',STRVAL(1:lnblnk(STRVAL))

        RETURN
      ELSE

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
C a portion of the text block. These working strings are used
C with the text version.
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

C Logic supports stepwise scrolling within the text. 243 label
C is the point for editing the initial portion of the text. 244 label
C is the point for editing 2nd tranche of text.
 243    CALL EASKSCMD(t72,PROMP1,PROMP2,'>',next,ISTRW,DSTR,
     &    'synp 1',IER,newnbhelp)

C lt72 is the number of characters found after editing. If this is
C less than 72 then cut characters from ipvsynop.
        lt72=max(1,lnblnk(t72))
        if(next)then

C Edit second section.
          CALL EASKS2CMD(t72a,PROMP1,PROMP2,'|<','>',
     &      inext,ISTRW,DSTR,ERMSG,IER,newnbhelp)
          lt72a=max(1,lnblnk(t72a))
          if(inext.eq.1)then

C If text to left requested go back to start.
            goto 243
          elseif(inext.eq.2)then

C If text to right requested, load 3rd portion and offer option to
C jump to earlier or last section of text.
            CALL EASKS2CMD(t72b,PROMP1,PROMP2,'|<','>|',
     &        inext,ISTRW,DSTR,ERMSG,IER,newnbhelp)
            lt72b=max(1,lnblnk(t72b))
            if(inext.eq.1)then

C Go back to start.
              next=.true.
              goto 243
            elseif(inext.eq.2)then

C Work with last portion of string, offer option to jump back
C to the start.
              CALL EASKSCMD(t72c,PROMP1,PROMP2,'|<..',next,ISTRW,
     &          DSTR,ERMSG,IER,newnbhelp)
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
     &      t72b(1:lt72b),' ',t72c(1:lt72c)
        else
          itwid=248-(lt72+lt72a+lt72b+3)
          write(STRVAL,'(7a)') t72(1:lt72),' ',t72a(1:lt72a),' ',
     &      t72b(1:lt72b),' ',t72c(1:itwid)
        endif
      endif
      return

   1  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKS248: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKS248: error composing message or prompt.'
        call edisp(iuout,outs)
      endif
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
C IER is the error state, if 0 then OK, if -3 then cancel pressed.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.

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

      CHARACTER*(*) PROMP1,PROMP2,CMD,ERMSG,STRVAL,DSTR
      CHARACTER WORD*72,A*1,LASTS*72,OUTSTR*124,prompt*144
      LOGICAL OK,CMDACT,DOK,unixok

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
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        lmmod=mmod
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif
        CALL askdialogcmd(PROMPT,STRVAL,CMD,IFLG)

C Debug.
C        write(6,*) 'askdialogcmd STRVAL:',STRVAL,' iflg ',IFLG

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(iflg.EQ.-3)THEN

C A cancel answer detected, set ier to -3 and return.
          CALL USRMSG('User requested a cancel for dialog',PROMPT,'-')
          ier=-3
          RETURN
        ELSEIF(iflg.EQ.-2)THEN
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
        ELSEIF(IFLG.EQ.2)THEN
          IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
            IF(LASTS(1:2).NE.'  ')THEN
              call isunix(unixok)
              if(unixok) WRITE(IUOUT,55)LASTS(1:last)
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
        IF(WORD.EQ.' '.or.IFLG.eq.-1)THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            call isunix(unixok)
            if(unixok) WRITE(IUOUT,55)LASTS(1:last)
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
            call isunix(unixok)
            if(unixok)then
              WRITE(IUOUT,55)LASTS(1:last)
   55         FORMAT(' The existing string (',a,') will be used.')
            endif
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
C incorporates the prompt, calling code defined cancel string, error
C messages and help facilities as follows:

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C CNCL is cancel command string, CNCLACT is logical if the
C cancel action is selected.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C  IER is the error state, if 0 then OK.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Assumption is that STRVAL will be up to 96 char wide. This
C should be generalized.
      SUBROUTINE EASKSCNCL(STRVAL,PROMP1,PROMP2,CNCL,CNCLACT,ISTRW,
     &  DSTR,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      CHARACTER*(*) PROMP1,PROMP2,CNCL,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124
      character outstr248*248,prompt*144
      LOGICAL OK,CNCLACT,DOK,unixok

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
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        lmmod=mmod
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif
        CALL askdialogcncl(PROMPT,STRVAL,CNCL,IFLG)

C Debug.
C        write(6,*) 'askdialogcncl STRVAL:',STRVAL(1:lnblnk(STRVAL)),
C     &    ' iflg ',IFLG

C PATCH to restore mmod (occassioanlly gets corrupted in linux)
        if(lmmod.ne.mmod)mmod=lmmod

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(iflg.EQ.-2)THEN
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
        ELSEIF(IFLG.EQ.2)THEN
          WRITE(OUTSTR248,55)LASTS(1:last)
          call edisp248(iuout,outstr248,90)
          write(STRVAL,'(a)') LASTS(1:last)
          CNCLACT=.TRUE.
          RETURN
        endif
        IF(WORD.EQ.' '.or.IFLG.eq.-1)THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            call isunix(unixok)
            if(unixok) WRITE(IUOUT,55)LASTS(1:last)
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
            call isunix(unixok)
            if(unixok)then
              WRITE(IUOUT,55)LASTS(1:last)
   55         FORMAT(' The existing string (',a,') will be used.')
            endif
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

      return
      end

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
C IER is the error state, if 0 then OK, if -3 then cancel button pressed.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Assumption is that STRVAL will be up to 96 char wide. This
C should be generalized.

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

      CHARACTER*(*) PROMP1,PROMP2,CMD,CMD2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,prompt*144
      LOGICAL OK,DOK,unixok

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
        IFLG=0
        WORD=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(newnbhelp)
        LN1=max(1,lnblnk(PROMP1))
        LN2=max(1,lnblnk(PROMP2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') PROMP2(1:LN2)
          else
            write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') PROMP1(1:LN1),' ',PROMP2(1:LN2)
        endif

C Debug.
C        write(6,*) '< askdialog2cmd prompt ',PROMPT(1:lnblnk(PROMPT))
C        write(6,*) '< askdialog2cmd string ',STRVAL(1:lnblnk(STRVAL))
        CALL askdialog2cmd(PROMPT,STRVAL,CMD,CMD2,IFLG)

C Debug.
C        write(6,*) 'askdialog2cmd STRVAL:',STRVAL,' iflg ',IFLG

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(STRVAL,WORD)

        IF(iflg.EQ.-3)THEN

C A cancel answer detected, set ier to -3 and return.
          CALL USRMSG('User requested a cancel for dialog',PROMPT,'-')
          ier=-3
          RETURN
        ELSEIF(iflg.EQ.-2)THEN
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
        ELSEIF(IFLG.EQ.2)THEN
          IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
            IF(LASTS(1:2).NE.'  ')THEN
              call isunix(unixok)
              if(unixok) WRITE(IUOUT,55)LASTS(1:last)
              STRVAL=LASTS(1:last)
              ICACT=1
              RETURN
            ENDIF
          ENDIF

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          ICACT=1
          RETURN
        ELSEIF(IFLG.EQ.3)THEN
          IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
            IF(LASTS(1:2).NE.'  ')THEN
              call isunix(unixok)
              if(unixok) WRITE(IUOUT,55)LASTS(1:last)
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
        IF(WORD.EQ.' '.or.IFLG.eq.-1)THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            call isunix(unixok)
            if(unixok) WRITE(IUOUT,55)LASTS(1:last)
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
            call isunix(unixok)
            if(unixok)then
              WRITE(IUOUT,55)LASTS(1:last)
   55         FORMAT(' The existing string (',a,') will be used.')
            endif
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
#include "epara.h"

      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      LOGICAL FOUND,ALL

      DIMENSION IVALS(NSTALT)
      dimension lista(35),listb(35)
      character lista*74,listb*74,KEY*1,outs*124
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STALT(*)
      CHARACTER H*72,SEL*40
      integer MVERT,IVERT ! max items and current menu item

#ifdef OSI
      integer impx,impy,iw
#else
      integer*8 impx,impy,iw
#endif

C Fill the help string buffer.
      call dupphelp(NHELP+1)

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
        LISTA(M+3)='                                                '
      else
        LISTA(M+3)='< index select                                  '
      endif
      LISTA(M+4)  ='? help                                          '
      LISTA(M+5)  ='- exit this menu                                '

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
C It is passed a prompt, menu title, string options (aopt ect.)
C default index (idindex) and current index (index) if zero then
C assumes no current selection. If user exits without selection then
C index is returned as zero if index was passed in as zero and
C is reset to index value if non-zero was passed.
C Automatic key characters are suppressed and are assumed to be
C passed within the parameter list text.
      SUBROUTINE MENUATOL(prompt,title,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,
     &  GOPT,HOPT,IOPT,JOPT,KOPT,LOPT,index,idindex,nhelp)

C Parameters passed.
      CHARACTER*(*) prompt,title,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT
      CHARACTER*(*) HOPT,IOPT,JOPT,KOPT,LOPT
      integer index   ! current index
      integer idindex ! default index
      integer nhelp   ! number of help lines to display

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      LOGICAL SELECT

      DIMENSION VERT(16)
      CHARACTER VERT*48,KEY*1,prompt2*36
      integer MVERT,IVERT ! max items and current menu item

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
        write(VERT(nopt+2),'(a,a,a)') '* default is option `',KEY,'`'
        if(ISTRW.lt.17)write(VERT(nopt+2),'(a,a,a)')
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
      CHARACTER*(*) MSG1,MSG2
      CHARACTER ANS*2,MSG3*124,opta*14,optb*14,optc*14
      character optd*2,opte*2,optf*2,optg*2,outs*124
      character prompt*144
      logical ok,dok,DEFLT

C Check if a default defined (via overload of nhelp). Later can
C make use of newnbhelp.
      DEFLT=.true.
      NHL=NHELP
      if (NHL.eq.-1) then
        DEFLT=.false.
        NHL=0
        call helpwithblank('Ok dialog',NHL,newnbhelp,ier)
      else
        call helpwithblank('Ok dialog',nhelp,newnbhelp,ier)
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

C Querry yes or no or default. Concat MSG1 & MSG2 together for prompt.
  19    continue
        LN1=max(1,lnblnk(MSG1))
        LN2=max(1,lnblnk(MSG2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') MSG2(1:LN2)
          else
            write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
        endif
        opta='yes'
        optb='no'
        optc='  '
        optd='  '
        opte='  '
        optf='  '
        optg='  '
        if (DEFLT) then
          if(DOK)then
            iw=1
            opta='default (yes)'
            optb='no'
            call espabcbox(prompt,opta,optb,optc,optd,opte,optf,optg,
     &        iw)
          else
            iw=2
            opta='yes'
            optb='default (no)'
            call espabcbox(prompt,opta,optb,optc,optd,opte,optf,optg,
     &        iw)
          endif
        else
          iw=0
          opta='yes'
          optb='no'
          optc='  '
          call espabcbox(prompt,opta,optb,optc,optd,opte,optf,optg,IW)
        endif
	
C Debug.
C        write(6,*) 'espabcbox selected iw ',iw,opta,optb,optc

        IF(IW.eq.1)THEN
          OK=.TRUE.
        ELSEIF(IW.eq.2)THEN
          OK=.FALSE.
        ELSEIF(DEFLT.and.IW.eq.3)THEN
          OK=DOK
        ELSEIF(IW.EQ.8.OR.IW.EQ.-8)THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
          CALL PHELPD('this multi-choice',newnbhelp,'-',0,0,IER)
          GOTO 19
        ENDIF
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
C EASKATOG choice of 3 - 7 items returning 1 to 7 according to
C which of the choices has been chosen.
C MSG1 and MSG2 are prompts (upto 124 char) to be printed.
C AOPT, BOPT & COPT, DOPT, EOPT, FOPT, GOPT are the text strings describing
C the options available, (these should not be too long).  If all optons
C are filled with text then all presneted.  If and option (beginning
C with FOPT and working backwards is a single blank space then fewer
C options are presented.
C In the case of a graphic menu us gtk functions will be called
C and the messages will appear in a pop-up dialogue box.
      SUBROUTINE EASKATOG(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,
     &  IWHICH,NHELP)

C Parameters passed.
      CHARACTER*(*) MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT
      integer iwhich  ! select passed back
      integer nhelp   ! number of lines of help to display

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER ANS*2,MSG3*124
      character prompt*144

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
           call helpwithblank('two choice dialog',nhelp,newnbhelp,ier)
           CALL PHELPD('two choice dialog',newnbhelp,'-',0,0,IER)
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
  19    LN1=max(1,lnblnk(MSG1))
        LN2=max(1,lnblnk(MSG2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') MSG2(1:LN2)
          else
            write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
        endif

C Passin the initial selection from the calling code if it is
C one of the buttons.
        if(IWHICH.ge.0.and.IWHICH.le.nopt) IW=IWHICH
        CALL espabcbox(prompt,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,IW)

C Debug.
C        write(6,*) 'call to espabcbox returned ',IW

        IF(IW.EQ.-8)THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
          call helpwithblank(prompt,nhelp,newnbhelp,ier)
          CALL PHELPD('this multi-choice',newnbhelp,'-',0,0,IER)
          GOTO 19
        ENDIF
        IWHICH=IW
      ENDIF
      RETURN

      END

C ******************** EASKDOZEN
C EASKDOZEN Generic choice of a dozen items returning 1 to 12 according to
C which of the choices has been chosen.
C MSG1 and MSG2 are prompts (upto 124 char) to be printed.
C AOPT, BOPT & COPT, DOPT, EOPT, FOPT, etc. are the text strings describing
C the options available, (these should not be too long).  If all optons
C are filled with text then all presneted.  If and option (beginning
C with FOPT and working backwards is a single blank space then fewer
C options are presented.
C In the case of a graphic menu us gtk functions will be called
C and the messages will appear in a pop-up dialogue box.
      SUBROUTINE EASKDOZEN(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,
     &  GOPT,HOPT,IOPT,JOPT,KOPT,LOPT,IWHICH,NHELP)

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER*(*) MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT
      CHARACTER*(*) HOPT,IOPT,JOPT,KOPT,LOPT
      CHARACTER ANS*2,MSG3*124
      character prompt*144

C See how many items there are to present.
      nopt=12
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
      LN2=max(1,LNBLNK(MSG2))
      if(ll.le.1)nopt=11
      if(lk.le.1)nopt=10
      if(lj.le.1)nopt=9
      if(li.le.1)nopt=8
      if(lh.le.1)nopt=7
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
        elseif(nopt.eq.8)then
          WRITE(MSG3,'(19a)')' ',MSG2(1:LN2),
     &    ' a) ',AOPT(1:la),' b) ',BOPT(1:lb),' c) ',COPT(1:lc),
     &    ' e) ',DOPT(1:ld),' f) ',EOPT(1:le),' g) ',FOPT(1:lf),
     &    ' i) ',GOPT(1:lg),' j) ',HOPT(1:lh),' ? '
        elseif(nopt.eq.9)then
          WRITE(MSG3,'(21a)')' ',MSG2(1:LN2),
     &    ' a) ',AOPT(1:la),' b) ',BOPT(1:lb),' c) ',COPT(1:lc),
     &    ' e) ',DOPT(1:ld),' f) ',EOPT(1:le),' g) ',FOPT(1:lf),
     &    ' i) ',GOPT(1:lg),' j) ',HOPT(1:lh),' k) ',IOPT(1:li),
     &    ' ? '
        elseif(nopt.eq.10)then
          WRITE(MSG3,'(23a)')' ',MSG2(1:LN2),
     &    ' a) ',AOPT(1:la),' b) ',BOPT(1:lb),' c) ',COPT(1:lc),
     &    ' e) ',DOPT(1:ld),' f) ',EOPT(1:le),' g) ',FOPT(1:lf),
     &    ' i) ',GOPT(1:lg),' j) ',HOPT(1:lh),' k) ',IOPT(1:li),
     &    ' l) ',JOPT(1:lj),' ? '
        elseif(nopt.eq.11)then
          WRITE(MSG3,'(25a)')' ',MSG2(1:LN2),
     &    ' a) ',AOPT(1:la),' b) ',BOPT(1:lb),' c) ',COPT(1:lc),
     &    ' e) ',DOPT(1:ld),' f) ',EOPT(1:le),' g) ',FOPT(1:lf),
     &    ' i) ',GOPT(1:lg),' j) ',HOPT(1:lh),' k) ',IOPT(1:li),
     &    ' l) ',JOPT(1:lj),' m) ',KOPT(1:lk),' ? '
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
        ELSEIF(ANS(1:1).EQ.'j'.OR.ANS(1:1).EQ.'J')THEN
          IWHICH=8
        ELSEIF(ANS(1:1).EQ.'k'.OR.ANS(1:1).EQ.'K')THEN
          IWHICH=9
        ELSEIF(ANS(1:1).EQ.'l'.OR.ANS(1:1).EQ.'L')THEN
          IWHICH=10
        ELSEIF(ANS(1:1).EQ.'m'.OR.ANS(1:1).EQ.'M')THEN
          IWHICH=11
        ELSEIF(ANS(1:1).EQ.'h'.OR.ANS(1:1).EQ.'H'.or.
     &         ANS(1:1).eq.'?')THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
           call helpwithblank(msg3,nhelp,newnbhelp,ier)
           CALL PHELPD('multi-choice dialog',newnbhelp,'-',0,0,IER)
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
  19    LN1=max(1,lnblnk(MSG1))
        LN2=max(1,lnblnk(MSG2))
	if((LN1+LN2+1).lt.144)then
          if(LN1.eq.1)then
            write(prompt,'(a)') MSG2(1:LN2)
          else
            write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
          endif
        else
          LN2=142-LN1
          write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
        endif

        CALL espdozenbox(prompt,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,
     &    HOPT,IOPT,JOPT,KOPT,LOPT,IW)

C Debug.
C        write(6,*) 'call to espdozenbox returned ',IW
        IF(IW.EQ.-8)THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
          call helpwithblank(prompt,nhelp,newnbhelp,ier)
          CALL PHELPD('this multi-choice',newnbhelp,'-',0,0,IER)
          GOTO 19
        ENDIF
        IWHICH=IW
      ENDIF
      RETURN

      END


C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EMPAGE(IPAG,IW,IEND)
C EMPAGE: Low level screen control for paging based on terminal MMOD.
C The available terminal see EPAGES.
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER DUMMY*1,blnk*2,blnk2*2,cont*8,prompt*72

      cont='continue'
      blnk='  '
      blnk2='  '

      IF(MMOD.EQ.-2)THEN

C MMOD=-2 TELETYPE: wait if full, page with blank line.
        IF(IEND.EQ.0)THEN
          IF(IPAG.EQ.1)THEN
            WRITE(IUOUT,301)
  301       FORMAT(/,' ',50('-'),'>>',$)
            READ(IUIN,'(A1)') DUMMY
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

C If scrolling text display (in GTK no need to clear it).
          IF(IPAG.EQ.1)then
            LIMIT=LIMTTY
          endif
          if(IW.eq.1)then
            write(prompt,'(a)') 'Continue...'
            CALL continuebox(prompt,cont)
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

C limtty is the initial request for text feedback height (in lines)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
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
        write(HEAD,'(a)') TITLE

C Take the pixel width and height passed.
C << figure out how to work with the positon on the screen >>
        if(iappwi.ge.100)then
          ihight=iapphi
          iwidth=iappwi
          imenuchw=menuchw
          ilimtty=limtty
          call createwin(iwidth,ihight,imenuchw,ilimtty,HEAD)
        else
          ihight=600
          iwidth=600
          imenuchw=menuchw
          ilimtty=limtty
          call createwin(iwidth,ihight,imenuchw,ilimtty,HEAD)
        endif

C Open the dialogue box and begin message.
        CALL msgbox('  ','  ')
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
        CALL EWMENU(NAME,ITEMS,NITMS,impx,impy,IRPX,IRPY,INO)
        return
      else
        IF(INO.GE.-1)GO TO 20

C Print out the menu. If text is > 36 char or there are few items
C then restrict to a single column. If less than 36 characters
C print double column.
        IF(IMLENG.GT.36.OR.NITMS.LE.6)THEN
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
C items passed in call to vwmenu.
      SUBROUTINE EWMENU(name,items,nitms,impx,impy,irpx,irpy,ino)
      character*(*) name, items(*)
      character itypes*40
      integer irpx,irpy

C To conform to Fortran -> C passing conventions on 32bit and 64bit
#ifdef OSI
      integer impx,impy,iw
#else
      integer*8 impx,impy,iw
#endif

      iw=LEN(items(1))
      call VWMENU(name,items,nitms,impx,impy,iw,irpx,irpy,ino)
      return
      end

C++++++++++ VWMENU +++++++++++++++++++++++++++++++++++++++++++++
C VWMENU: Is the binding to C function for menu dialogue.  It
C allows the string widths to be variable widths.
      SUBROUTINE VWMENU(name,items,nitms,impx,impy,iw,irpx,irpy,ino)
      character*(*) name, items(*)
      integer irpx,irpy

C To conform to Fortran -> C passing conventions on 32bit and 64bit
#ifdef OSI
      integer impx,impy,iw
#else
      integer*8 impx,impy,iw
#endif

C Fill the help string buffer (need to pass NHELP).
      NHELP=20
      call dupphelp(NHELP+1)

C Setup and display menu.
      call espmenuinit(name)
      do 10 I=1,nitms
        call espmenuitems(items(I),I)
 10   continue
      call espmenu(INO)

C Overload INO with click position and button.  A nine digit number is
C used the first digit is the mouse button and the next 4 represent the
C x pixel and the last 4 the y pixel.
      if (ino.lt.0) then
C        ino=-100000000*iuresp-10000*irpx-irpy

C Debug.
C        write(6,*) 'EWMENU (fortran) ',iw,irpx,irpy,ino,ipflg,iuresp

      endif

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
      character prompt*144

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
          LN1=max(1,LNBLNK(MSG1))
          LN2=max(1,LNBLNK(MSG2))
	  if((LN1+LN2+1).lt.144)then
            if(LN1.eq.1)then
              write(prompt,'(a)') MSG2(1:LN2)
            else
              write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
            endif
          else
            LN2=142-LN1
            write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
          endif

C Debug.
C          write(6,*) '< msgbox prompt ',prompt(1:lnblnk(prompt))
          CALL continuebox(prompt,cont)
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

C << Place to try pop-up version of msgbox. >>
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
      character prompt*144

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
          LN1=max(1,LNBLNK(MSG1))
          LN2=max(1,LNBLNK(MSG2))
	  if((LN1+LN2+1).lt.144)then
            if(LN1.eq.1)then
              write(prompt,'(a)') MSG2(1:LN2)
            else
              write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
            endif
          else
            LN2=142-LN1
            write(prompt,'(3a)') MSG1(1:LN1),' ',MSG2(1:LN2)
          endif

C Debug.
C          write(6,*) '< msgbox prompt ',prompt(1:lnblnk(prompt))
          CALL continuebox(prompt,cont)
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
C For type 8 manages the text which is passed to inserttext for
C treatment as a scrolling window.
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
          CALL inserttext(WWMSG)
      ELSEIF(MMOD.EQ.-6)THEN

C Write this to error channel if user specified channel = ICOUT
C otherwise redirect to user display.
        IF(ITRU.EQ.ICOUT)THEN
          WRITE(ICOUT,'(A)',iostat=ios,err=1)MSG(1:lnm)
        ELSE
          WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
        ENDIF
      ELSE

C Write this to standard text window.
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
      call dupphelp(NHELP)
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


C ************************ PHELPD
C PHELPD displays the current contents of common pophelp in a form
C appropriate to the current terminal type. MSG is a short descriptive
C string for the subject. NHELP is the number of lines to be displayed.
C IBX & IBY are the preferred coords of the lower left corner if in
C terminal type 8.
      SUBROUTINE PHELPD(MSG,NHELP,OTHER,IBX,IBY,IER)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      dimension lh(30)
      CHARACTER*(*) MSG,OTHER
      CHARACTER h*72,outs*73,lh*73
      character IA*3
      integer IBX1,IBY1

C Set coordinates IBX and IBY to zero (so parameter not needed).
      IBX1=0; IBY1=0
      IPM = 1

C dh is duplicate help list.

      IER=0
      IF(NHELP.eq.0)THEN
        IF(MMOD.EQ.8)THEN
          if(LNBLNK(MSG).le.50)then
            LN=max(1,LNBLNK(MSG))
            WRITE(H(1),64,iostat=ios,err=1)MSG(1:LN)
          else
            WRITE(H(1),64,iostat=ios,err=1)MSG(1:50)
          endif
          CALL PHELPW(1,IHW,IER)
          call dupphelp(1)
          ipflg=0
          CALL egphelpscroll(IBX,IBY,IPFLG,0,iuresp)
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
        call dupphelp(NHELP)
        CALL egphelpscroll(IBX1,IBY1,IPFLG,0,iuresp)
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
          return
        else
          if(IPM.eq.1)then
            call edisp(iuout,
     &        ' Options:  a) -, b) next, c) continue ')
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
          elseif(IPM.eq.MPM)then
            call edisp(iuout,
     &        ' Options:  a) previous, b) -, c) continue ')
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
          else
            call edisp(iuout,
     &        ' Options:  a) previous, b) next, c) continue ')
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
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


C ************************ dupphelp(nh)
C dupphelp(nh) copies current pop-uphelp common into a string array so that
C C code does not destroy it. Note: this uses the esp_text.c version
C rather than the esru_x.c version.
      SUBROUTINE dupphelp(NH)

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      CHARACTER H*72,dh*72

      call clrhelp
      DO 50 I=1,NH
        dh=h(I)
        call updhelp(dh)
   50 CONTINUE
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


C ************* GETZONENAMES
C GETZONENAMES calls the c function putzonename to fill the C zonenames variable.
      subroutine GETZONENAMES
#include "building.h"

C Get precz common block from geometry.h
#include "geometry.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character t12*12

      do 10 I=1,NCOMP
        t12=zname(I)
        call putzonename(t12,I)
 10   continue

      return
      end

C ************* GDUPDATE
C GDUPDATE Update the graphical display of the model.
      subroutine GDUPDATE
#include "building.h"
#include "prj3dv.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      common/rpath/path
      COMMON/PREC8/SLAT,SLON
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

C ITDSP labels toggle: all surf + obs = 0, all surf = 1, extrn = 2
C       partn = 3 ,similar = 4, surfs + obs+ ground = 5, ground only = 6
C ITBND bounds toggle: static = 0, optimum = 1, zone focus = 2
C ITEPT is not used,
C ITZNM zone name toggle: display = 0, hidden = 1
C ITSNM surface name toggle: display = 0, hidden = 1
C ITORG origin toggle: display = 0, hidden = 1
C ITSNR surf normal toggle: display = 0, hidden = 1.
C ITOBS obstruction toggle: not yet enabled.
C ITHLS highlight toggle: normal 0, constr 1, trans/opaq 2, part atrib 3
C ITGRD grid toggle: display = 0, hidden = 1
C ITVNO vertex toggle: display = 0, hidden = 1
C ITPPSW current view - perspective/plan/south/west
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW

      logical MODIFY,MODLEN,MODBND
      DIMENSION MTHNAM(12)
      CHARACTER DESCRH*5,DESCRD*5,path*72
      CHARACTER MTHNAM*3,ETEXT*60,outs*124
C      character tmode*8,longtfile*72

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/
#ifdef OSI
      integer ivt2,ivt3,ivt4  ! for use with viewtext
#else
      integer*8 ivt2,ivt3,ivt4  ! for use with viewtext
#endif

C Refresh the image.
      itrc=0
      IUF=IFIL+1
      MODLEN=.TRUE.
      MODBND=.TRUE.
      MODIFY=.TRUE.

C Debug
C      write(6,*)ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
C     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW

      if(ITPPSW.eq.0)then

C If perspective view do the normal wireframe calls.
        if(ITHLS.eq.1)then

C If ITHLS set to 1 then ask about which construction.
C But this is disabled because epkmlc tries to use the current
C main menu structure which causes a GTK fault. Need an alternative
C list management for this.
          call usrmsg('Hilight by construction is not yet working',
     &                'this will be added in a later version.','P')
C          CALL EPKMLC(ISEL,'Select a construction to hilight.',' ',IER)
C          ITHLZ=ISEL
C          MODIFY=.TRUE.
        endif

        CALL INLNST(1)
        CALL ADJVIEW(IER)
        MODLEN=.FALSE.
        MODBND=.FALSE.
        MODIFY=.FALSE.
      elseif(ITPPSW.eq.1)then

C User toggle to a plan view.
        call PLELEV('P')
      elseif(ITPPSW.eq.2)then

C User toggle to a south view.
        call PLELEV('S')
      elseif(ITPPSW.eq.3)then

C User toggled to an east view.
        call PLELEV('E')
      elseif(ITPPSW.eq.4)then

C Perspective view from the sun is handled by other code.
        call edisp(iuout,'Use the pull-down menu for views from sun.')
      endif

      return
      end

C ************* EVSET
C EVSET provides a dummy setup environment for wire frame view.
      SUBROUTINE EVSET(act,IER)

      character act*1
      integer ier
      return
      END


C ********* dummy subroutines and functions to match what is found
C in esru_fc.F.  May need to include explicit typing in some subroutines.

C opencpw place copyright button on screen
      subroutine opencpw
      return
      end

C opensetup place setup button on screen
      subroutine opensetup
      return
      end

C updcapt() notify level for capture buttons
      subroutine updcapt(iavail)
      return
      end

C updazi() notify level for azimuth button
      subroutine updazi(iavail)
      return
      end

C feedbox() open feedback background box
      subroutine feedbox(menu_char,id_lines,igw,igh)
      return
      end

C opengdisp opens a scrolling text display area.
      subroutine opengdisp(menu_char,idispl_l,idial_l,igdw,igdh)
      return
      end

C Return string indicating xlibs
      subroutine getXlibs( cLibrary )

      character*3 cLibrary
      write(cLibrary, '(A)') "GTK"

      return
      end
