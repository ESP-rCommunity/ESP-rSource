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


C This file contains the following subroutines.
C  EASKI:   Ask user for an integer with prompt, error messages & range
C           checking as passed parameters.
C  EASKR:   Ask user for a real number with prompt, error messages & range
C           checking as passed parameters.
C  EASKE:   Ask user for a real in exponential format, otherwise as easkr.
C  EASKF:   Ask user for a file name with prompt, error messages & help.
C  EASKS:   Ask user for a string with prompt, error messages & help.
C  EASKS248: Edit a long (248 char) string in sections.
C  EASKS496: Edit a long (496 char) string in sections.
C  EASKSCMD Ask user for a string with prompt, alt, error messages & help.
C  EASKSCNCL Asking the user for a text string with prompt cancel error & help.
C  EASKS2CMD Ask user for a string with prompt, 2 alts, error messages & help.

C  EPICKS:  Allows several selections to be made from an array of strings.
C  MENUATOL Single item menu pick with items passed via parameters.
C  ASKOK:   Generic yes/no/default facility returning OK as a logical parameter.
C  EASKOK:   Generic yes/no facility returning OK as a logical parameter.
C  EASKATOG: Generic A/B/C... choice facility returning 1-7 according
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
C  EDISPxtr: Generic send text to scrolling display (text or graphic)
C           without trim.
C  EDISP2tr: Generic send text to scrolling display (text or graphic)
C           with 2 trailing whitespaces for markdown.
C  PHELPD:  Displays the current contents of common pophelp.
C  PHELPW:  Returns the width IWH of the longest popup help string.

C  EVSET:   provides setup environment for wire frame view w/in prj.
C Startbuffer: wrapper around call to win3dclr
C popupimage: display image with documentation (dummy of GTK version)

C ******************** EASKI ********************
C Asks the user for an integer number.
C   IVAL            - returned integer.
C   PROMP1 & PROMP2 - prompts to be presented to the user.
C   MINV & MAXV     - the minimum and maximum allowed values.
C   MINACT & MAXACT - actions to take if the range is exceeded
C                     (if W accept but warn user, if F refuse value
C                     and ask again, if - no range checking).
C   IDEFLT          - default value (if D or d entered).
C   ERMSG           - string to ppended to a read error
C   IER             - error state (if 0 OK, if -3 cancel).
C   NHELP           - number of help lines (if ?, H or h entered).
C Example:
C        IZONE=1
C        CALL EASKI(IZONE,' ','Zone number?',
C     &             1,'F',MCOM,'W',1,'zone number',IER,2)

      SUBROUTINE EASKI(IVAL,PROMP1,PROMP2,MINV,MINACT,
     &                 MAXV,MAXACT,IDEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*10,STR1*10,STR2*10
      CHARACTER ask*10,OUTSTR*124,DSTR*124,dh*72,cancel*6,outs*124
      LOGICAL OK
      integer ivalold

#ifdef OSI
      integer idef,iquery,iiwidth
#else
      integer*8 idef,iquery,iiwidth
#endif

      IER=0

C If IVAL outwith range, set to default.
      if(MINACT.EQ.'F'.and.IVAL.LT.MINV)IVAL=IDEFLT
      if(MAXACT.EQ.'F'.and.IVAL.GT.MAXV)IVAL=IDEFLT

C Clear string buffers and instantiate 'ask'.
   20 WORD=' '
      WRITE(ask,'(I5)',iostat=ios,err=1)IVAL

C Remember input value in case of a cancel.
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
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
        idef=0
        iquery=0
        iiwidth=10

C Include a cancel option.
        cancel='cancel'
        CALL openaskcnclbox(PROMP1,PROMP2,cancel,iiwidth)
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
          call usrmsg(' ',' ','-')  ! clear lower display
          IMFS=lastmenufont         ! reset font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        elseif(idef.eq.2)then

C Cancel button was pressed, reset ival as ivalold and mark as cancel. 
          IVAL=ivalold
          ier=-3
          IMFS=lastmenufont    ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        ENDIF
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing integer value.
          CALL INTSTR(IVAL,STR2,IW2,IER)
          WRITE(OUTSTR,55,iostat=ios,err=1)STR2(1:IW2)
          CALL USRMSG(' ',OUTSTR,'-')
          IMFS=lastmenufont    ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        ENDIF
      ELSE

C In text mode, remind user of current value.
        write(OUTSTR,'(3a)')'(currently: ',STR2(1:IW2),')'
        call edisp(iuout,OUTSTR)
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,24,END=666)WORD
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
        CALL USRMSG(' ','Invalid input, try again.','-')
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
          CALL EASKOK(OUTSTR,'Is this OK?',OK,newnbhelp)
          IVAL=IVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg(' ',' ','-')
          RETURN
        ELSEIF(MINACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter.','W')
          GOTO 20
        ELSEIF(MINACT.EQ.'-')THEN
          IVAL=IVALT
          IF(MMOD.EQ.8)call usrmsg(' ',' ','-')
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
          CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
          IVAL=IVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg(' ',' ','-')
          RETURN
        ELSEIF(MAXACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter.','W')
          GOTO 20
        ELSEIF(MAXACT.EQ.'-')THEN
          IVAL=IVALT
          IF(MMOD.EQ.8)call usrmsg(' ',' ','-')
          RETURN
        ENDIF
      ELSE
        IVAL=IVALT
      ENDIF

C Return to calling module.
      IF(MMOD.EQ.8)call usrmsg(' ',' ','-')
      RETURN

999   CALL USRMSG(' ','Invalid input, retry.','-')
      GOTO 20

   1  if(IOS.eq.2)then
        write(outs,*) 'EASKI: permission error composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKI: error composing message or prompt.'
        call edisp(iuout,outs)
      endif
      return
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKI: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END

C ******************** EASKR ********************
C Asks the user for a real number.
C Parameters are as defined in EASKI.

      SUBROUTINE EASKR(RVAL,PROMP1,PROMP2,RMIN,MINACT,
     &                 RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*16,STR2*16
      CHARACTER ask*16,OUTSTR*124,DSTR*124,dh*72,outs*124
      LOGICAL OK,close

C If RVAL is outwith range, set to default.
      if(MINACT.EQ.'F'.and.RVAL.LT.RMIN)RVAL=DEFLT
      if(MAXACT.EQ.'F'.and.RVAL.GT.RMAX)RVAL=DEFLT

C Check that value is not trivially close to zero.
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

C Begin with prompts.
      IF(MMOD.EQ.8)THEN

C If in graphic mode use graphic faciltiies to edit the number
C and switch to fixed width font for editing.
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
        idef=0
        iquery=0
        iwidth=16

        CALL openaskbox(PROMP1,PROMP2,iwidth)
        WORD=' '
        CALL PHELPW(nhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,nhelp,IHW)
        CALL askdialog(ask,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks.
        call c2fstr(ask,WORD)
        IF(idef.EQ.1)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,' ')
          call edisp(iuout,DSTR)
          RVAL=DEFLT
          call usrmsg('  ','  ','-')  ! clear lower display
          IMFS=lastmenufont           ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        ENDIF
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use existing real value.
          CALL REL16STR(RVAL,STR2,IW2,IER)
          WRITE(OUTSTR,55,iostat=ios,err=1)STR2(1:IW2)
          CALL USRMSG(' ',OUTSTR,'-')
          IMFS=lastmenufont    ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        ENDIF
      ELSE

C In text mode, remind user of current value.
        write(OUTSTR,'(3a)')'(currently: ',STR2(1:IW2),')'
        call edisp(iuout,OUTSTR)
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,24,END=666)WORD
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
            CALL PHELPD('ask user real',nhelp,'-',0,0,IER)
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
          CALL EASKOK(OUTSTR,'Is this OK?',OK,nhelp)
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
          CALL EASKOK(OUTSTR,'Is this ok?',OK,nhelp)
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKR: EOF detected, error in input commands.'
      call pauses(1)
      STOP
      END

C ******************** EASKE ********************
C Ask user for an real number in exponential format.
C Parameters are as defined in EASKI.

      SUBROUTINE EASKE(RVAL,PROMP1,PROMP2,RMIN,MINACT,
     &                 RMAX,MAXACT,DEFLT,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER MINACT*1,MAXACT*1,A*1,WORD*16,STR1*10,STR2*10
      CHARACTER ask*16,OUTSTR*124,DSTR*124,dh*72,outs*124
      LOGICAL OK,close

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
   27 FORMAT('The default value (',a,') will be used.')

C Begin with prompts.
C If in terminal type 8 (dialogue box) use ww facilities to get the
C string to convert and switch to fixed width font for editing.
      IF(MMOD.EQ.8)THEN
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
        idef=0
        iquery=0
        iwidth=16
        CALL openaskbox(PROMP1,PROMP2,iwidth)
        WORD=' '
        CALL PHELPW(nhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,nhelp,IHW)
        CALL askdialog(ask,idef,iquery)

C Strip off the last character (end of line mark) as well as any
C leading blanks from string returned.
        call c2fstr(ask,WORD)
        IF(idef.EQ.1)THEN

C A 'D' or 'd' detected, set to default value.
          call edisp(iuout,' ')
          call edisp(iuout,DSTR)
          RVAL=DEFLT
          call usrmsg('  ','  ','-')  ! clear lower display
          IMFS=lastmenufont           ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        ENDIF
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing real value.
          CALL EXPSTR(RVAL,STR2,IW2,IER)
          WRITE(OUTSTR,55,iostat=ios,err=1)STR2(1:IW2)
          CALL USRMSG(' ',OUTSTR,'-')
          IMFS=lastmenufont    ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        ENDIF
      ELSE

C In text mode, remind user of current value.
        write(OUTSTR,'(3a)')'(currently: ',STR2(1:IW2),')'
        call edisp(iuout,OUTSTR)
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,24,END=666)WORD
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
            CALL PHELPD('ask user real',nhelp,'-',0,0,IER)
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
        CALL USRMSG(' ','Did not understand input, try again.','-')
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
          CALL EASKOK(OUTSTR,'Is this OK?',OK,nhelp)
          RVAL=RVALT
          IF(.NOT.OK)GOTO 20
          IF(MMOD.EQ.8)call usrmsg('  ','  ','-')
          RETURN
        ELSEIF(MINACT.EQ.'F')THEN
          CALL USRMSG(OUTSTR,'Please re-enter.','W')
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
          CALL EASKOK(OUTSTR,'Is this ok?',OK,nhelp)
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

C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKE: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END

C ******************** EASKF ********************
C Asks the user for a file name.
C Parameters are as defined in EASKI.

      SUBROUTINE EASKF(FILEN,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,FILEN,DSTR
      CHARACTER WORD144*144,A*1,LASTS*144,OUTSTR*124,dh*72,outs*124,
     &          cncl*6
      LOGICAL OK

#ifdef OSI
      integer IISTRW,idef,iquery
#else
      integer*8 IISTRW,idef,iquery
#endif

   20 IER=0

C Remember the file name in case of a cancel.
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

C Get user input via a dialogue box (terminal type 8) or text window read.
C The file name is limited to ISTRW characters.
      IF(MMOD.EQ.8)THEN
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
        idef=0
        iquery=0
        cncl='cancel'
        iistrw=int(istrw)
        CALL openaskcnclbox(PROMP1,PROMP2,cncl,IISTRW)
        WORD144=' '
        CALL PHELPW(newnbhelp,IHW,IER)
        call dupphelp(dh)
        call updhelp(dh,newnbhelp,IHW)
        lmmod=mmod
        CALL askcncldialog(FILEN,cncl,idef,iquery)

        if(lmmod.ne.mmod)mmod=lmmod  ! Restore mmod in case value lost.

C Strip off the last character (end of line mark) as well as any
C leading blanks.
        call c2fstr(FILEN,WORD144)

        IF(idef.EQ.1)THEN
          LN=max(1,LNBLNK(DSTR))
          WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
     &      DSTR(1:LN),'`.'
          CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
          IF(OK)THEN
            FILEN=DSTR(1:LN)
            call usrmsg('  ','  ','-')  ! clear lower display
            IMFS=lastmenufont           ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
            RETURN
          ELSE
            GOTO 20
          ENDIF
        ELSEIF(idef.EQ.2)THEN
          IER=-3
          call usrmsg(' ',' ','-')  ! clear lower display
          IMFS=lastmenufont         ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
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
            IMFS=lastmenufont    ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
            RETURN
          ELSE
            CALL USRMSG('Current file name is blank!',
     &                  'Please re-enter.','W')
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
        READ(IUIN,'(A144)',END=666)WORD144
        A=WORD144(1:1)
        IF(lnblnk(WORD144).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
     &        DSTR(1:LN),'`.'
            CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
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
            CALL USRMSG('Current file name is blank!',
     &                  'Please re-enter.','W')
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKF: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END

C ******************** EASKS ********************
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
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72,outs*124
      LOGICAL OK

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
C the text window. Note that the string read in is limited to
C ISTRW characters wide. If in terminal type 8 use dialogue box
C and switch to fixed width font to support editing.
      IF(MMOD.EQ.8)THEN
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
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
          CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            call usrmsg('  ','  ','-')  ! clear lower display
            IMFS=lastmenufont           ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
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
            IMFS=lastmenufont    ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
            RETURN
          ELSE
            CALL USRMSG('Current string is blank!',
     &                  'Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:ISTRW)
        call usrmsg('  ','  ','-')
        IMFS=lastmenufont    ! reset to proportional font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
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
        READ(IUIN,'(A)',END=666)WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)',iostat=ios,err=1)'The default is `',
     &        DSTR(1:LN),'`.'
            CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
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
            CALL USRMSG('Current string is blank!',
     &                  'Please re-enter.','W')
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKS: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END

C ******************** EASKS248 ********************
C EASKS248 edit a long (248 char) string in sections.

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C IER is the error state, if 0 then OK.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
      SUBROUTINE EASKS248(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,
     &   NHELP)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      character t72*72,t72a*72,t72b*72,t72c*72
      LOGICAL next
      integer ISTRW

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

      return
      end

C ******************** EASKS496 ********************
C Edit a long (496 char) string in sections.

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C IER is the error state, if 0 then OK.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.

      SUBROUTINE EASKS496(STRVAL,PROMP1,PROMP2,ISTRW,DSTR,ERMSG,IER,
     &   NHELP)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STRVAL,DSTR
      character t72*72,t72a*72,t72b*72,t72c*72,t72d*72
      character t72e*72,t72f*72,t72g*72
      LOGICAL next
      integer ISTRW

C Find breakpoints near 72 144 216 248  characters.
C If STRVAL null make into a blank string.
      ipall=lnblnk(STRVAL)
      if(ipall.le.1) STRVAL='  '
      ip72=iprevblnk(STRVAL,72)
      ip144=iprevblnk(STRVAL,ip72+71)
      ip216=iprevblnk(STRVAL,ip144+71)
      ip288=iprevblnk(STRVAL,ip216+71)
      ip360=iprevblnk(STRVAL,ip288+71)
      ip432=iprevblnk(STRVAL,ip360+71)
      ip496=iprevblnk(STRVAL,496)

C Create working strings for each section of text. Either as blanks
C in the case of a short initial string or in lengths of ~72 char.
C Remember the lengths in lt72 lt72a etc. in case user edits only
C a portion of the text block.
      t72=' ';  t72a=' '; t72b=' '; t72c=' '
      t72d=' '; t72e=' '; t72f=' '; t72g=' '
      write(t72,'(a)') STRVAL(1:ip72)
      if(ip144.gt.ip72)write(t72a,'(a)') STRVAL(ip72+1:ip144)
      lt72a=max(1,lnblnk(t72a))
      if(ip216.gt.ip144)write(t72b,'(a)') STRVAL(ip144+1:ip216)
      lt72b=max(1,lnblnk(t72b))
      if(ip288.gt.ip216)write(t72c,'(a)') STRVAL(ip216+1:ip288)
      lt72c=max(1,lnblnk(t72c))
      if(ip360.gt.ip288)write(t72d,'(a)') STRVAL(ip288+1:ip360)
      lt72d=max(1,lnblnk(t72d))
      if(ip432.gt.ip360)write(t72e,'(a)') STRVAL(ip360+1:ip432)
      lt72e=max(1,lnblnk(t72e))
      if(ip496.gt.ip432)write(t72f,'(a)') STRVAL(ip432+1:ip496)
      lt72f=max(1,lnblnk(t72f))

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
C jump to earlier or next of text.
          CALL EASKS2CMD(t72b,PROMP1,PROMP2,'|<','>',
     &      inext,ISTRW,DSTR,ERMSG,IER,NHELP)
          lt72b=max(1,lnblnk(t72b))
          if(inext.eq.1)then

C Go back to start.
            next=.true.
            goto 243
          elseif(inext.eq.2)then

C If text to right requested, load 4th portion and offer option to
C jump to earlier or next of text.
            CALL EASKS2CMD(t72c,PROMP1,PROMP2,'|<','>',
     &        inext,ISTRW,DSTR,ERMSG,IER,NHELP)
            lt72c=max(1,lnblnk(t72c))
            if(inext.eq.1)then

C Go back to start.
              next=.true.
              goto 243
            elseif(inext.eq.2)then

C If text to right requested, load 5th portion and offer option to
C jump to earlier or next of text.
              CALL EASKS2CMD(t72d,PROMP1,PROMP2,'|<','>',
     &          inext,ISTRW,DSTR,ERMSG,IER,NHELP)
              lt72d=max(1,lnblnk(t72d))
              if(inext.eq.1)then

C Go back to start.
                next=.true.
                goto 243
              elseif(inext.eq.2)then

C If text to right requested, load 6th portion and offer option to
C jump to earlier or next of text.
                CALL EASKS2CMD(t72e,PROMP1,PROMP2,'|<','>',
     &            inext,ISTRW,DSTR,ERMSG,IER,NHELP)
                lt72e=max(1,lnblnk(t72e))
                if(inext.eq.1)then

C Go back to start.
                  next=.true.
                  goto 243
                elseif(inext.eq.2)then

C Work with last portion of string, offer option to jump back
C to the start.
                  CALL EASKSCMD(t72f,PROMP1,PROMP2,'|<..',next,ISTRW,
     &              DSTR,ERMSG,IER,NHELP)
                  lt72f=max(1,lnblnk(t72f))
                  if(next)then
                    goto 243
                  endif
                endif
              endif
            endif
          endif
        endif
      endif

C When writing out the combined text, put a space between each section.
      call usrmsg(' ',' ','-')
      itwid=lt72+lt72a+lt72b+lt72c+lt72d+lt72e+lt72f+6
      if(itwid.le.496)then
        write(STRVAL,'(13a)') t72(1:lt72),' ',t72a(1:lt72a),' ',
     &    t72b(1:lt72b),' ',t72c(1:lt72c),' ',t72d(1:lt72d),' ',
     &    t72e(1:lt72e),' ',t72f(1:lt72f)
      else
        itwid=496-(lt72+lt72a+lt72b+lt72d+lt72e+lt72f+5)
        write(STRVAL,'(13a)') t72(1:lt72),' ',t72a(1:lt72a),' ',
     &    t72b(1:lt72b),' ',t72c(1:lt72c),' ',t72d(1:lt72d),' ',
     &    t72e(1:lt72e),' ',t72f(1:itwid)
      endif

      return
      end

C ******************** EASKSCMD ********************
C Asks the user for a text string which incorporates the prompt,
C alternative command, error messages and help facilities as follows.

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C CMD is an alternative command string, CMDACT is logical if the
C command action is selected.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C IER is the error state, if 0 then OK.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Internal buffer size assumes that STRVAL is less than 96 characters.

C Example:
C    ....
C test getting a string.
C        XNAME=' '
C        DNAME=' '
C        CALL EASKSCMD(XNAME,' ','Site name?','Use map',CACT,
C     &                13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKSCMD(STRVAL,PROMP1,PROMP2,CMD,CMDACT,ISTRW,DSTR,
     &  ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CMD,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72
      LOGICAL OK,CMDACT
      integer ISTRW

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
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
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
          CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            call usrmsg('  ','  ','-')  ! clear lower display
            IMFS=lastmenufont           ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
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
              IMFS=lastmenufont    ! reset to proportional font
              ITFS=lasttextfont
              IFS=lastbuttonfont
              call userfonts(IFS,ITFS,IMFS)
              RETURN
            ENDIF
          ENDIF

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          CMDACT=.TRUE.
          IMFS=lastmenufont    ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        endif
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
            STRVAL=LASTS(1:last)
            CMDACT=.FALSE.
            IMFS=lastmenufont    ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
            RETURN
          ELSE
            CALL USRMSG('Current string is blank!',
     &                  'Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:ISTRW)
        CMDACT=.FALSE.
        IMFS=lastmenufont    ! reset to proportional font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        RETURN
      ELSE

C Text mode.
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A72)',END=666)WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)') 'The default is `',
     &        DSTR(1:LN),'`.'
            CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
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
            CALL USRMSG('Current string is blank!',
     &                  'Please re-enter.','W')
            GOTO 20
          ENDIF
        ELSE

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          RETURN
        ENDIF
      ENDIF
      RETURN
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKSCMD: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END

C ******************** EASKSCNCL ********************
C Ask the user for a text string which incorporates the prompt,
C cancel, error messages and help facilities as follows.

C STRVAL is the string returned, ISTRW is its length.
C PROMP1 & PROMP2 are the prompts using the same syntax as USRMSG.
C CNCL is cancel command string, CNCLACT is logical if the
C command action is selected.
C If a space is typed then STRVAL is not changed.
C ERMSG is a string placed at the top of the selection menu and to
C any range checking or read errors to identify the value.
C IER is the error state, if 0 then OK.
C NHELP is the number of help lines if '?','H','h' is typed
C by the user.
C DSTR is a string to use as a default.
C Internal buffer size assumes that STRVAL is less than 96 characters.

C Example:
C    ....
C test getting a string.
C        XNAME=' '
C        DNAME=' '
C        CALL EASKSCNCL(XNAME,' ','Site name?','Use map',CACT,
C     &                 13,DNAME,'test sites',IER,1)
C        CALL USRMSG(' ',' selected string is:'//XNAME,'-')

      SUBROUTINE EASKSCNCL(STRVAL,PROMP1,PROMP2,CNCL,CNCLACT,ISTRW,
     &  DSTR,ERMSG,IER,NHELP)

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CNCL,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72
      character outstr248*248
      LOGICAL OK,CNCLACT

#ifdef OSI
      integer IISTRW,idef,iquery
#else
      integer*8 IISTRW,idef,iquery
#endif

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
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
        idef=0
        iquery=0
        iistrw=int(istrw)
        CALL openaskcnclbox(PROMP1,PROMP2,CNCL,IISTRW)
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
          CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            call usrmsg('  ','  ','-')  ! clear lower display
            IMFS=lastmenufont    ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
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
          call usrmsg('  ','  ','-')  ! clear lower display
          IMFS=lastmenufont           ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        endif
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
            STRVAL=LASTS(1:last)
            CNCLACT=.FALSE.
            IMFS=lastmenufont    ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
            RETURN
          ELSE
            CALL USRMSG('Current string is blank!',
     &                  'Please re-enter.','W')
            GOTO 20
          ENDIF
        ENDIF

C Must have input a character string.
        STRVAL=WORD(1:ISTRW)
        CNCLACT=.FALSE.
        IMFS=lastmenufont    ! reset to proportional font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        RETURN
      ELSE

C Text mode.
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A72)',END=666)WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)') 'The default is `',
     &        DSTR(1:LN),'`.'
            CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKSCNCL: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END

C ******************** EASKS2CMD ********************
C Ask the user for a text string which incorporates the prompt, two
C alternative commands, error messages and help facilities as follows.

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

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS

      dimension dh(60)
      CHARACTER*(*) PROMP1,PROMP2,CMD,CMD2,ERMSG,STRVAL,DSTR
      CHARACTER WORD*96,A*1,LASTS*96,OUTSTR*124,dh*72
      LOGICAL OK
      integer ISTRW
#ifdef OSI
      integer iistrw,idef,iquery
#else
      integer*8 iistrw,idef,iquery
#endif

   20 IER=0
      ICACT=0
      WORD=' '
      last=max(1,lnblnk(STRVAL))
      LASTS=STRVAL(1:last)

C Generate custom help text.
      call helpwithblank(ermsg,nhelp,newnbhelp,ier)

C Get user input via dialogue box or Fortran read in
C the text window.
C Note that the string read in is limited to ISTRW characters wide.
C If in terminal type 8, use dialogue box.
      IF(MMOD.EQ.8)THEN
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.4) IFS=0
        if(IFS.eq.5) IFS=1
        if(IFS.eq.6) IFS=2
        if(IFS.eq.7) IFS=3
        call userfonts(IFS,ITFS,IMFS)
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
          CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
          IF(OK)THEN
            STRVAL=DSTR(1:LN)
            call usrmsg('  ','  ','-')  ! clear lower display
            IMFS=lastmenufont           ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
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
              IMFS=lastmenufont    ! reset to proportional font
              ITFS=lasttextfont
              IFS=lastbuttonfont
              call userfonts(IFS,ITFS,IMFS)
              RETURN
            ENDIF
          ENDIF

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          ICACT=1
          IMFS=lastmenufont    ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
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
              IMFS=lastmenufont    ! reset to proportional font
              ITFS=lasttextfont
              IFS=lastbuttonfont
              call userfonts(IFS,ITFS,IMFS)
              RETURN
            ENDIF
          ENDIF

C Must have input a character string.
          STRVAL=WORD(1:ISTRW)
          ICACT=2
          IMFS=lastmenufont    ! reset to proportional font
          ITFS=lasttextfont
          IFS=lastbuttonfont
          call userfonts(IFS,ITFS,IMFS)
          RETURN
        endif
        IF(WORD.EQ.' ')THEN

C A blank, carriage return or line feed encountered, assume user
C wishes to use prior/existing string.
          IF(LASTS(1:2).NE.'  ')THEN
            WRITE(IUOUT,55)LASTS(1:last)
            STRVAL=LASTS(1:last)
            ICACT=0
            IMFS=lastmenufont    ! reset to proportional font
            ITFS=lasttextfont
            IFS=lastbuttonfont
            call userfonts(IFS,ITFS,IMFS)
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
        IMFS=lastmenufont    ! reset to proportional font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        RETURN
      ELSE

C Text mode.
        CALL USRMSG(PROMP1,PROMP2,'?')
        READ(IUIN,'(A72)',END=666)WORD
        A=WORD(1:1)
        IF(lnblnk(WORD).EQ.1)THEN
          IF(A.EQ.'D'.OR.A.EQ.'d')THEN

C A 'D' or 'd' detected, set to default.
            LN=max(1,LNBLNK(DSTR))
            WRITE(OUTSTR,'(3a)') 'The default is `',
     &        DSTR(1:LN),'`.'
            CALL EASKOK(OUTSTR,'Is this ok?',OK,newnbhelp)
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKS2CMD: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END


C ******************** EPICKS ********************
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
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      LOGICAL FOUND,ALL

      DIMENSION IVALS(NSTALT)
      dimension lista(35),listb(35)
      character lista*74,listb*74,KEY*1,outs*124
      CHARACTER*(*) PROMP1,PROMP2,ERMSG,STALT(*)
      CHARACTER SEL*54     ! string for heading
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
      helpinsub='lib'
      helptopic='pickfromstringarray'
      write(outs,'(3a)') helpinsub,' ',helptopic

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

C Generate text for title (SEL).
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
      LISTA(M+5)  ='- exit menu'

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
        INPICK=1
  93    CALL EASKI(INPICK,' ',' No of items to pick?',
     &     1,'F',IALLOW,'F',1,'script no of items',IER,nbhelp)
        if(IER.NE.0)goto 93
        DO 94 I=1,INPICK
  95      write(outs,'(A,I2)') ' Index (number) of item ',I
          CALL EASKI(IV,outs,' ',1,'F',NSTALT,'F',I,'script itm',
     &      IER,nbhelp)
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

C ******************** MENUATOL ********************
C Presents a list of up to a dozen items to select via a
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
      VERT(nopt+4)  ='- exit menu'

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

C ******************** ASKOK ********************
C Generic choice facility returning logical variable
C from a yes no (with default indicated) prompt (supports help).
C In the case of a graphic menu the messages will appear in a
C dialogue box at the bottom of the graphic window.
C If NHELP=-1 there is no default option.

      SUBROUTINE ASKOK(MSG1,MSG2,OK,DOK,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      CHARACTER*(*) MSG1,MSG2
      CHARACTER ANS*2,MSG3*124,outs*124
      logical ok,dok,DEFLT
      integer IWI  ! for radio button
      integer lastfont,iuse

C Check if a default defined and use local var for NHELP.
      DEFLT=.true.
      NHL=NHELP
      if (NHL.eq.-1) then
        DEFLT=.false.
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
        READ(IUIN,'(A2)',IOSTAT=IOS,ERR=1,END=666)ANS
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

C Save off current menu font and if mono-space set to proportional.
        lastfont=IFS
        if(IFS.ge.0.and.IFS.le.3)then
          IUSE=IFS+4
          call userfonts(IUSE,ITFS,IMFS)
        endif

C Querry yes or no or default.
        IWI=1
        if (DEFLT) then
          if(DOK)then
            call EASKATOG(MSG1,MSG2,'yes (d)','no',' ',' ',' ',
     &      ' ',' ',IWI,newnbhelp)
          else
            call EASKATOG(MSG1,MSG2,'yes','no (d)',' ',' ',' ',
     &      ' ',' ',IWI,newnbhelp)
          endif
        else
          call EASKATOG(MSG1,MSG2,'yes','no',' ',' ',' ',
     &      ' ',' ',IWI,newnbhelp)
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
        IFS=lastfont    ! re-establish font
        call userfonts(IFS,ITFS,IMFS)
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'ASKOK: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END  ! of ASKOK


C ******************** EASKOK ********************
C EASKOK Generic choice facility returning logical variable
C from a yes no prompt.
C In the case of a graphic menu the messages will appear in a
C dialogue box at the bottom of the graphic window.

      SUBROUTINE EASKOK(MSG1,MSG2,OK,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      CHARACTER*(*) MSG1,MSG2
      CHARACTER ANS*2,MSG3*124,outs*124
      logical ok
      integer IWI  ! for radio button
      integer lastfont,iuse

C Check if a default defined and use local var for NHELP.
      call helpwithblank('Ok dialog',nhelp,newnbhelp,ier)

C Print out message according to the combination of strings passed. Use
C the dialogue box if terminal type 8.
      IF(MMOD.LT.8)THEN

C Setup a single line menu to pick from via EASKS.
   21   LN=max(1,LNBLNK(MSG2))
        WRITE(MSG3,'(2a)')MSG2(1:LN),' [Y]es [N]o ?'
        LN1=max(1,lnblnk(MSG1))
        LN3=max(1,lnblnk(MSG3))
        ans = '  '
        WRITE(IUOUT,'(a)')MSG1(1:LN1)
        WRITE(IUOUT,'(a)')MSG3(1:LN3)

C Querry yes or no.
        READ(IUIN,'(A2)',IOSTAT=IOS,ERR=1,END=666)ANS
        IF(ANS(1:1).EQ.'Y'.OR.ANS(1:1).EQ.'y')THEN
          OK=.TRUE.
        ELSEIF(ANS(1:1).EQ.'N'.OR.ANS(1:1).EQ.'n')THEN
          OK=.FALSE.
        ELSE
          CALL USRMSG(' ','You must make a choice!','-')
          GOTO 21
        ENDIF
      ELSEIF(MMOD.EQ.8)THEN

C Save off current menu font and if mono-space set to proportional.
        lastfont=IFS
        if(IFS.ge.0.and.IFS.le.3)then
          IUSE=IFS+4
          call userfonts(IUSE,ITFS,IMFS)
        endif

C Querry yes or no.
        IWI=1
        call EASKATOG(MSG1,MSG2,'yes','no',' ',' ',' ',
     &    ' ',' ',IWI,newnbhelp)
        IF(IWI.eq.1)THEN
          OK=.TRUE.
        ELSEIF(IWI.eq.2)THEN
          OK=.FALSE.
        ENDIF
        call usrmsg('  ','  ','-')
        IFS=lastfont    ! re-establish font
        call userfonts(IFS,ITFS,IMFS)
      ENDIF
     
      RETURN

   1  if(IOS.eq.2)then
        write(outs,*) 
     &  'EASKOK: permissions exception while composing prompt.'
        call edisp(iuout,outs)
      else
        write(outs,*) 'EASKOK: error composing prompt.'
        call edisp(iuout,outs)
      endif
      RETURN
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKOK: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END  ! of EASKOK

C ******************** EASKATOG ********************
C EASKATOG Generic choice of 3 - 7 items returning 1 to 7 according to
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

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      CHARACTER ANS*2,MSG3*248
      integer lastfont,iuse

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

C Generate custom help text.
        call helpwithblank(msg3,nhelp,newnbhelp,ier)
        ans = '  '
        call lusrmsg(msg1,msg3,'?')
        read(iuin,'(A2)',END=666)ANS

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
           call helpwithblank('multi-choice dialog',nhelp,newnbhelp,ier)
           CALL PHELPD('multi-choice dialog',newnbhelp,'-',0,0,IER)
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

C Save off current menu font and if mono-space set to proportional.
        lastfont=IFS
        if(IFS.ge.0.and.IFS.le.3)then
          IUSE=IFS+4
          call userfonts(IUSE,ITFS,IMFS)
        endif

C Querry which one.
   19   CALL abcdefbox(MSG1,MSG2,AOPT,BOPT,COPT,DOPT,EOPT,FOPT,GOPT,IW)
        IF(IW.EQ.8)THEN

C A probable request for help has been received. Print out the lines
C of help supplied and then provide the prompt again.
          call helpwithblank('multi-choice dialog',nhelp,newnbhelp,ier)
          CALL PHELPD('multi-choice',newnbhelp,'-',0,0,IER)
          GOTO 19
        ENDIF
        IWHICH=IW
     
        IFS=lastfont    ! re-establish font
        call userfonts(IFS,ITFS,IMFS)
      ENDIF
      RETURN

C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EASKATOG: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END  ! of EASKATOG

C ******************** EMPAGE ********************
C Low level screen control for paging based on terminal MMOD.
C The available terminal see EPAGES.

      SUBROUTINE EMPAGE(IPAG,IW,IEND)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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
            READ(IUIN,'(A1)',END=666)DUMMY
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
      goto 999
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EMPAGE: EOF detected, error in input commands.'
      call pauses(1)
      STOP

  999 CONTINUE
      END

c ******************** ELINC ********************
C Controls scratch pad output for text screens which returns:
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
C type -6 = shell script mode.
C type -2 = text mode with page control.
C type -1 = text mode with no page control.
C type  8 = graphic mode.
C iappwi is the requested pixel width, iapphi is the requested pixel height
C iappx & iappy are the upper-left position on the monitor <not yet implemented >
C menuchw is the initial request for menu width (in characters)
      SUBROUTINE EPAGES(MODEL,IIN,IOUT,iappwi,iapphi,iappx,iappy,
     &  menuchw,TITLE,lntitle)

C Parameters
      integer lntitle
      character (len=lntitle) :: TITLE

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      common/gfont/ifs,itfs,imfs
      CHARACTER HEAD*255
#ifdef OSI
      integer iimenu,ilf,igfw,igfh,ild,igdw,igdh
#else
      integer*8 iimenu,ilf,igfw,igfh,ild,igdw,igdh
#endif

      CHARACTER*5 MM
      integer ifont   ! to pass to winfnt
      integer immod   ! to pass to jwinint

c If MMOD set to valid index, assume it, dont ask.
      MM='     '
  10  IF(MM(1:5).EQ.'-6   '.OR.MODEL.EQ.-6)THEN
        MMOD=-6
        LIMTTY=30  ! allow a-z plus some headers and footers
        LIMIT=30
        RETURN
      ELSEIF(MM(1:5).EQ.'-2   '.OR.MODEL.EQ.-2)THEN
        MMOD=-2
        LIMTTY=30
        LIMIT=30
        RETURN
      ELSEIF(MM(1:5).EQ.'-1   '.OR.MODEL.EQ.-1)THEN
        MMOD=-1
        LIMTTY=30
        LIMIT=30
        RETURN
      ELSEIF(MM(1:5).eq.'8    '.OR.MODEL.EQ.8)THEN
        MMOD = 8
        write (HEAD,'(a)') TITLE(1:lnblnk(TITLE))

C Pass application size via sizehwxy.
        if(iappwi.ge.100)then
          ihight=iapphi
          iwidth=iappwi
          call sizehwxy(ihight,iwidth,iappx,iappy)
        endif

C Setup the X11 structures.
        immod=8
        call jwinint(immod,HEAD)

C Setup the ranges of zone/grey & interface colors here rather
C than within the main fortran code.
C        call setzscale()
C        call setgscale()
C        call setcscale()

C Set an initial font.
        ifont=1
        call winfnt(ifont)

C Open the dialogue box and begin message then open the
C feedback box with call to feedbox, passing it the initial
C menu character width mechchw. Note, nothing uses igfw or
C igfh. Then call opengdisp.
        iimenu=menuchw; ilf=2; ild=LIMTTY
        call feedbox(iimenu,ilf,igfw,igfh)
        call opengdisp(iimenu,ild,ilf,igdw,igdh)
        CALL msgbox(' ',' ')
        RETURN
      ELSE
        WRITE(IOUT,200)
 200    FORMAT(' Terminal mode (type ? for options): ')
        READ(IIN,101,ERR=500,IOSTAT=IOS)MM
 101    FORMAT(A5)
        IF(MM(1:5).EQ.'-6   '.OR.MM(1:5).EQ.'-2   '.OR.
     &     MM(1:5).EQ.'-1   '.OR.MM(1:5).EQ.'8    ')THEN
          goto 10
        ELSEIF(MM(1:1).EQ.'?')THEN
          WRITE(IOUT,102)
 102      FORMAT(/
     &  ' This program can be be executed in different',/,
     &  ' modes as follows.',//,
     &  ' -6 shell script mode',/,
     &  ' -2 text with page control',/,
     &  ' -1 text mode',/,
     &  '  8 graphic mode',/)
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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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
      ITLENG=max(1,LNBLNK(NAME))  ! Title length
      ISLENG=0
      IMLENG=0
      DO IL=1,NITMS
        ISLENG=max(1,LNBLNK(ITEMS(IL)))
        IF(ISLENG.GT.IMLENG)IMLENG=ISLENG
      ENDDO

C See if an even or odd number of items in list.
      im=MOD(NITMS,2)
      odd=.false.
      if(im.eq.1) odd=.true.

C Generate menu depending on terminal.
      if(MMOD.eq.8)then

C Bitmapped screen :  do calls for raster menu, 0,0 are passed
C to define std upper right position, IRPX,IRPY not used.
        irpx=0; irpy=0; impx=0; impy=0
        CALL EWMENU(NAME,ITEMS,NITMS,impx,impy,irpx,irpy,INO)
        return
      else
        IF(INO.GE.-1)GO TO 20

C Print out the menu. If text is > 36 char or there are few items
C then restrict to a single column. If less than 36 characters
C print double column.
        IF(IMLENG.GT.36.OR.NITMS.LE.6)THEN
          LN=max(1,LNBLNK(ITEMS(1)))
          WRITE(IUOUT,'(/,2a)',iostat=ios,err=1)NAME(1:ITLENG),': '
          DO K=1,NITMS
            LN=max(1,LNBLNK(ITEMS(K)))
            WRITE(IUOUT,'(2a)',iostat=ios,err=2)tmpa(1:2),ITEMS(K)(1:LN)
          ENDDO
        ELSEIF(IMLENG.LT.24)THEN
          MNULEN=NITMS/2
          WRITE(IUOUT,'(/,2a)',iostat=ios,err=1)NAME(1:ITLENG),': '
          WRITE(IUOUT,'(4a)',iostat=ios,err=2)tmpa(1:2),
     &      ITEMS(1)(1:IMLENG),'   ',ITEMS(MNULEN+1)(1:IMLENG)
          DO K=2,MNULEN
            WRITE(IUOUT,'(4a)',iostat=ios,err=2)tmpa(1:2),
     &        ITEMS(K)(1:IMLENG),'   ',ITEMS(K+MNULEN)(1:IMLENG)
          ENDDO
          IF(odd)THEN
            LN=max(1,LNBLNK(ITEMS(NITMS)))
            WRITE(IUOUT,'(2a)',iostat=ios,err=2)tmpa(1:2),
     &        ITEMS(NITMS)(1:LN)
          ENDIF
        ELSEIF(IMLENG.GE.24.and.IMLENG.LE.36)THEN
          MNULEN=(NITMS/2)
          WRITE(IUOUT,'(2a)',iostat=ios,err=2)NAME(1:ITLENG),':'
          DO K=1,MNULEN
            WRITE(IUOUT,'(4a)',iostat=ios,err=2)tmpa(1:2),
     &        ITEMS(K)(1:IMLENG),'   ',ITEMS(K+MNULEN)(1:IMLENG)
          ENDDO
          IF(odd)THEN
            LN=max(1,LNBLNK(ITEMS(NITMS)))
            WRITE(IUOUT,'(2a)',iostat=ios,err=2)tmpa(1:2),
     &        ITEMS(NITMS)(1:LN)
          ENDIF
        ENDIF

C Solicit Reply.
  20    LN=max(1,LNBLNK(NAME))
        WRITE(IUOUT,104)NAME(1:LN)
 104    FORMAT(/,' ',a,':?> ',$)
        READ(IUIN,105,iostat=ios,ERR=20,END=666)JJ
 105    FORMAT(A1)
        ICUR=IFIRST(JJ)

C TEST REPLY IS IN MENU
        DO JNO=1,NITMS
          INO=JNO
          IF(ICUR.EQ.IFIRST(ITEMS(JNO)))RETURN
        ENDDO
        DO JNO=1,NITMS
          INO=JNO
          IF(ICUR-32.EQ.IFIRST(ITEMS(JNO)))RETURN
        ENDDO
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EMENU: EOF detected, error in input commands.'
      call pauses(1)
      STOP

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

C Optional to check extents of the menu about to be drawn and recover
C information about the font widths which will be used.  Of interest
C if the _ character of the font is particularly wide.
C      call extentsvwmenu(name,iw,ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul)
C      write(6,'(a,6i5)') name,iw,ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul
      
C Call to display.
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
C     integer iwipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul
#else
      integer*8 impxe,impye,iwe,irpxe,irpye,inoe,ipflg,iuresp  ! for use with evwmenu
      integer*8 impx,impy,iw
C     integer*8 ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul
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

C Optional to check extents of the menu about to be drawn and recover
C information about the font widths which will be used.  Of interest
C if the _ character of the font is particularly wide.
C      call extentsvwmenu(name,iwe,ipixwthma,ipixwthll,
C     &  ivfw,ivfwsp,ivfwul)
C      write(6,*) name,iwe,ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul

C Call to display.
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

      return
      end ! of VWMENU

C++++++++++ ULMENU +++++++++++++++++++++++++++++++++++++++++++++
C ULMENU: Returns an underline string for use in menus based on
C the current font (proportional or mono-spaced.
      SUBROUTINE ULMENU(name,items,nitms,ulstring)
      integer MMOD,LIMIT,LIMTTY
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      character*(*) name, items(*)
      character*(*) ulstring
      character itypes*40
      integer ipix,numul,loop,lnul

C To conform to Fortran -> C passing conventions on 32bit and 64bit
C computers cast the parameters passed into ewmenu to local variables
C of either integer or integer*8 to match the long int variables in
C the C function evwmenu.  Do reverse cast after return from evwmenu.
#ifdef OSI
      integer iw,irpxe,irpye,inoe,ipflg,iuresp  ! for use with evwmenu
      integer impx,impy,ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul
#else
      integer*8 iw,irpxe,irpye,inoe,ipflg,iuresp  ! for use with evwmenu
      integer*8 impx,impy,ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul
#endif

C Deal with pure-text case. Assume two blanks and one blank at far right.
      if(MMOD.ne.8)then
        numul=(menuchw-3)
        numulcount=numul
        lnul=LEN(ulstring)
        do loop=1,lnul  ! loop through characters
          if(loop.eq.1.or.loop.eq.2)then
            write(ulstring(loop:loop),'(a)') ' '  ! 2 leading blanks
          else
            if(numulcount.ge.1)then
              write(ulstring(loop:loop),'(a)') '_'  ! an underline
              numulcount=numulcount-1
            else
              write(ulstring(loop:loop),'(a)') ' '  ! 2 leading blanks
            endif
          endif
        enddo
        write(6,*) ulstring
        return
      endif

C Append a null terminator to the title string if possible.
      iw=LEN(items(1))
      write(itypes,9)
   9  format(40('-'))
      call updmenu(items,itypes,nitms,iw)

C Check extents.
      call extentsvwmenu(name,iw,ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul)
      write(6,*) name,iw,ipixwthma,ipixwthll,ivfw,ivfwsp,ivfwul

C Instantiate string with two initial spaces and enough to stick within
C the ipixwthll length.
      ipix=int(ivfwsp)*3  ! width of 3 spaces
      numul=(ipixwthll-ipix)/int(ivfwul)
      numulcount=numul
      lnul=LEN(ulstring)
      write(6,*) ipix,numul
      do loop=1,lnul  ! loop through characters
        if(loop.eq.1.or.loop.eq.2)then
          write(ulstring(loop:loop),'(a)') ' '  ! 2 leading blanks
        else
          if(numulcount.ge.1)then
            write(ulstring(loop:loop),'(a)') '_'  ! an underline
            numulcount=numulcount-1
          else
            write(ulstring(loop:loop),'(a)') ' '  ! 2 leading blanks
          endif
        endif
      enddo

      return
      end

C ******************** EMKEY ********************
C Returns a key character for a menu item (a-z) based on the array
C index of the item.  Uses ICHPK(26), 'a','b'...

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
      ENDIF
      RETURN
      END


c ******************** USRMSG ********************
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
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/OUTIN/IUOUT,IUIN,IEOUT
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
          call edisp(iuout,' Incorrect syntax in USRMSG!')
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

C If fixed width font switch to proportional.
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.0) IFS=4
        if(IFS.eq.1) IFS=5
        if(IFS.eq.2) IFS=6
        if(IFS.eq.3) IFS=7
        call userfonts(IFS,ITFS,IMFS)

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
          CALL msgbox(' ',' Incorrect syntax in USRMSG!')
          call pauses(1)
        ENDIF

        IF(LEVEL.EQ.'F'.OR.LEVEL.EQ.'f')then
          call pauses(2)
          STOP
        endif
        IMFS=lastmenufont    ! reset font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        RETURN
      ENDIF

   1  if(IOS.eq.2)then
        call edisp(IUOUT,'USRMSG: permission error composing prompt!')
      else
        call edisp(IUOUT,'USRMSG: error composing prompt!')
      endif
      return
      END

c ******************** LUSRMSG ********************
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
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/OUTIN/IUOUT,IUIN,IEOUT
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
          call edisp(iuout,' Incorrect syntax in USRMSG!')
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

C If fixed width font switch to proportional.
        lastmenufont=IMFS
        lastbuttonfont=IFS
        lasttextfont=ITFS
        if(IFS.eq.0) IFS=4
        if(IFS.eq.1) IFS=5
        if(IFS.eq.2) IFS=6
        if(IFS.eq.3) IFS=7
        call userfonts(IFS,ITFS,IMFS)

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
        IMFS=lastmenufont    ! reset font
        ITFS=lasttextfont
        IFS=lastbuttonfont
        call userfonts(IFS,ITFS,IMFS)
        RETURN
      ENDIF

   1  if(IOS.eq.2)then
        call edisp(IUOUT,'LUSRMSG: permission error composing prompt.')
      else
        call edisp(IUOUT,'LUSRMSG: error composing prompt.')
      endif
      return
      END

C ********************* EDISP ********************
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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/textbuf/dispbuf(500)
      common/textbufl/indexbuf,lnbuf(500)
      CHARACTER*(*) MSG
      CHARACTER WWMSG*144,dispbuf*144
      logical unixok

C Function determining if HOT3000 features enabled?
      logical bH3KExtentionsActive

      ICOUT=0

C Create matching string to pass to wwlib.c and hold in dispbuf.
C Use logic similar to that in egdisp.
      lnm=max0(1,lnblnk(MSG))
      lcc = MIN0(lnm,144)
      if(lcc.eq.0) lcc=1
      write(WWMSG,'(a)',iostat=ios,err=1)MSG(1:lcc)

      if(indexbuf.lt.500)then
        indexbuf=indexbuf+1
        lnbuf(indexbuf)=lcc
        dispbuf(indexbuf)='  '
        write(dispbuf(indexbuf),'(a)') MSG(1:lcc)
      else
        do 42 i=1,499
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
          WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
        ENDIF
      ELSE

C Write to standard text window.
        WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)

C Write console output (ITRU=6) to file.       
        if(ITRU.eq.6 .and. bH3KExtentionsActive() )then
            call redir_console_output_to_file(MSG(1:lnm)//CHAR(0))
        endif
      ENDIF

      RETURN
   1  call isunix(unixok)
      if(.NOT.unixok) return  ! If DOS, return because of lack of I/O channel
      if(IOS.eq.2)then
        if(lnm.le.1)then
          write(6,*) 'edisp: permission error writing blank/null!'
        else
          write(6,*) 'edisp: permission error writing text ',MSG,'!'
        endif
      else
        if(lnm.le.1)then
          write(6,*) 'edisp: error writing blank/null!'
        else
          write(6,*) 'edisp: error writing text ',MSG,'!'
        endif
      endif
      return
      END

C ********************* EDISPxtr *********************
C A generic routine which displays lines of text passed to it
C in a format depending on the terminal type (no trailing whitespace trim
C useful for markdown documents):
C For types -1 -2 9 does a fortran write to channel IUOUT,
C For types -6 writes to ICOUT,
C For type 8 manages the text which is passed to egdisp for
C treatment as a scrolling window.
C Use egdispclr to clear the display window on a new page.
C The maximum width of text which can be accommodated to a graphic
C context is 124 characters.

      SUBROUTINE EDISPxtr(ITRU,MSG)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/textbuf/dispbuf(500)
      common/textbufl/indexbuf,lnbuf(500)
      CHARACTER*(*) MSG
      CHARACTER WWMSG*144,dispbuf*144
      logical unixok

C Function determining if HOT3000 features enabled?
      logical bH3KExtentionsActive
C Temporarily set ICOUT.
      ICOUT=0

C Create matching string to pass to wwlib.c and hold in dispbuf.
C Use logic similar to that in egdisp.
      lnm=max0(1,lnblnk(MSG))
      lcc = MIN0(lnm,144)
      if(lcc.eq.0) lcc=1
      write(WWMSG,'(a)',iostat=ios,err=1)MSG(1:lcc)

      if(indexbuf.lt.500)then
        indexbuf=indexbuf+1
        lnbuf(indexbuf)=lcc
        dispbuf(indexbuf)='  '
        write(dispbuf(indexbuf),'(a)') MSG(1:lcc)
      else
        do 42 i=1,499
          dispbuf(i)=dispbuf(i+1)
          lnbuf(i)=lnbuf(i+1)
  42    continue
        lnbuf(indexbuf)=lcc
        dispbuf(indexbuf)='  '
        write(dispbuf(indexbuf),'(a)') MSG(1:lcc)
      endif
      IF(MMOD.EQ.8.AND.ITRU.NE.IUOUT)THEN

C If trace channel etc then just write to file with no trim.
        WRITE(ITRU,'(A)',iostat=ios,err=1)MSG
      ELSEIF(MMOD.EQ.8.AND.ITRU.EQ.IUOUT)THEN
          LINE=LIMTTY-LIMIT
          LIMIT=LIMIT-1
          if (LIMIT.lt.1) LIMIT=1
          call espad(limit,limtty,line)
          CALL egdisp(WWMSG,LINE)
      ELSEIF(MMOD.EQ.-6)THEN

C Write this to error channel if user specified channel = ICOUT
C with no trim otherwise redirect to user display.
        IF(ITRU.EQ.ICOUT)THEN
          WRITE(ICOUT,'(A)',iostat=ios,err=1)MSG
        ELSE
          WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
        ENDIF
      ELSE

C Write this to standard text window.
        WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
c.......write console output (ITRU=6) to file       
        if(ITRU.eq.6 .and. bH3KExtentionsActive() )then
            call redir_console_output_to_file(MSG(1:lnm)//CHAR(0))
        endif
      ENDIF

      RETURN
   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      if(IOS.eq.2)then
        if(lnm.le.1)then
          write(6,*) 'edispxtr: permission error writing blank/null!'
        else
          write(6,*) 'edispxtr: permission error writing text ',MSG,'!'
        endif
      else
        if(lnm.le.1)then
          write(6,*) 'edispxtr: error writing blank/null!'
        else
          write(6,*) 'edispxtr: error writing text ',MSG,'!'
        endif
      endif
      return
      END


C ********************* EDISP2tr *********************
C A generic routine which displays lines of text passed to it
C in a format depending on the terminal type with 2 trailing spaces for
C use in markdown documents to signal return:
C For types -1 -2 9 does a fortran write to channel IUOUT,
C For types -6 writes to ICOUT,
C For type 8 manages the text which is passed to egdisp for
C treatment as a scrolling window.
C Use egdispclr to clear the display window on a new page.
C The maximum width of text which can be accommodated to a graphic
C context is 124 characters.

      SUBROUTINE EDISP2tr(ITRU,MSG)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/textbuf/dispbuf(500)
      common/textbufl/indexbuf,lnbuf(500)
      CHARACTER*(*) MSG
      CHARACTER WWMSG*144,dispbuf*144
      logical unixok

C Function determining if HOT3000 features enabled?
      logical bH3KExtentionsActive
C Temporarily set ICOUT.
      ICOUT=0

C Create matching string to pass to wwlib.c and hold in dispbuf.
C Use logic similar to that in egdisp but add 2 whitespace.
      lnm=max0(1,lnblnk(MSG)); lnm=lnm+2
      lcc = MIN0(lnm,144)
      if(lcc.eq.0) lcc=3
      write(WWMSG,'(a)',iostat=ios,err=1)MSG(1:lcc)

      if(indexbuf.lt.500)then
        indexbuf=indexbuf+1
        lnbuf(indexbuf)=lcc
        dispbuf(indexbuf)='  '
        write(dispbuf(indexbuf),'(a)') MSG(1:lcc)
      else
        do 42 i=1,499
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
          WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)
        ENDIF
      ELSE

C Write this to standard text window.
        WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)

C Write console output (ITRU=6) to file.      
        if(ITRU.eq.6 .and. bH3KExtentionsActive() )then
            call redir_console_output_to_file(MSG(1:lnm)//CHAR(0))
        endif
      ENDIF

      RETURN
   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      if(IOS.eq.2)then
        if(lnm.le.1)then
          write(6,*) 'edisp: permission error writing blank/null!'
        else
          write(6,*) 'edisp: permission error writing text ',MSG,'!'
        endif
      else
        if(lnm.le.1)then
          write(6,*) 'edisp: error writing blank/null!'
        else
          write(6,*) 'edisp: error writing text ',MSG,'!'
        endif
      endif
      return
      END

C ******************** proftxdump ********************
C Write current text buffer to an appropraite file.

      subroutine proftxdump()
#include "building.h"
#include "model.h"
#include "help.h"

C Path to model and command line file (if any).
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      common/rpath/path

C Indicator of possible focus zone.
      logical browse
      common/user/browse
      common/textbuf/dispbuf(500)
      common/textbufl/indexbuf,lnbuf(500)

      character path*72,uname*24,fs*1
      character sstr*144
      character lltmp*144
      character dispbuf*144,outs*124
      logical unixok,there
      integer ltf  ! position of last character in string.
      integer iw   ! for user selection

      if(MMOD.ne.8)return

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif
      uname=' '
      sstr=' '
      call usrname(uname)
      helpinsub='lib'  ! set for subroutine
      helptopic='write_current_text'
      call gethelptext(helpinsub,helptopic,nbhelp)

C There are several location options to present to the user
C depending on whether we are working in the model folder or
C remotely, including the users home folder. Also take into
C account whether we are browsing the model or own it.
  11  if(browse)then

C If browsing the model the output file should be dumped to
C the users home folder (a place we know we can write to).
        LNU=max(1,lnblnk(upath))
        if(cfgroot(1:2).eq.'  ')then
          LN=max(1,lnblnk(uname))
          write(sstr,'(4a)') upath(1:LNU),fs,uname(1:LN),'.txt'
        else
          LN=max(1,lnblnk(cfgroot))
          write(sstr,'(4a)') upath(1:LNU),fs,cfgroot(1:LN),'.txt'
        endif
        call edisp(iuout,'  ')  ! echo a black line
      else
        iw=0
        if(path.ne.'./'.and.path.ne.' ')then
          call edisp(iuout,'  ')  ! echo a black line
          write(outs,'(A,A)') 'The current path is: ',path
          call edisp(iuout,outs)
          CALL EASKABC('You are working in a remote  folder.',
     &      'Options:','use remote  folder','use local folder',
     &      'cancel',IW,nbhelp)
          if(iw.eq.1)then
            LN=max(1,lnblnk(cfgroot))
            write(sstr,'(4a)') path(1:lnblnk(path)),fs,
     &        cfgroot(1:LN),'.txt'
          elseif(iw.eq.2)then
            LN=max(1,lnblnk(cfgroot))
            write(sstr,'(3a)') './',cfgroot(1:LN),'.txt'
          elseif(iw.eq.3)then
            return
          endif
        else
          LN=max(1,lnblnk(cfgroot))
          write(sstr,'(3a)') './',cfgroot(1:LN),'.txt'
        endif
      endif

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path. sstr is the suggested name and
C lltmp is the file name returned by the user selection.
      lltmp='  '
      CALL EASKXORGTKF(sstr,' ','Export file name?',
     &    'textbuffer.txt',lltmp,'buffer file',IER,nbhelp)

C If user request jump back and re-display the menu.
      if(ier.eq.-3)then
        return  ! cancel detected, return.
      endif
      if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
        goto 11
      endif

C Attempt to write out file.
      ltf=max(1,LNBLNK(lltmp))
      iuf=ifil+1
      there=.false.
      INQUIRE(FILE=lltmp(1:ltf),EXIST=there)
      if(there)then
        close(iuf)
        open(iuf,file=lltmp(1:ltf),position='APPEND',
     &    status='UNKNOWN',err=1)
      else
        close(iuf)
        open(iuf,file=lltmp,status='UNKNOWN',err=1)
      endif
      if(indexbuf.gt.1)then
        do 42 i=1,indexbuf
          write(iuf,'(a)',iostat=ios,err=2) dispbuf(i)(1:lnbuf(i))
  42    continue
      endif
      close(iuf)
      return

   1  if(IOS.eq.2)then
        call usrmsg('Permissions open/write dump file...',lltmp,'W')
      else
        call usrmsg('Could not open/write dump file...',lltmp,'W')
      endif
      return
   2  if(IOS.eq.2)then
        call usrmsg('Permissions for append to dump file ',lltmp,'W')
      else
        call usrmsg('Could not append to dump file ',lltmp,'W')
      endif
      return

      end

C ********************* profgrdump *********************
C Graphics capture proforma.

      subroutine profgrdump()

#include "building.h"
#include "model.h"
#include "help.h"

C Path to model and command line file (if any).
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN,IEOUT
      common/rpath/path

C Indicator of possible focus zone.
      logical browse
      common/user/browse

C Graphic capture, window dump: label, command.
      character gprlbl*20,gprcmd*48
      common/gprint/gprlbl,gprcmd

      character path*72,uname*24,fs*1
      character sstr*144
      character lltmp*144,outs*124
      character gcmd*144
      integer ltf  ! position of last character in string.
      integer iw   ! for user selection

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
      sstr=' '
      call usrname(uname)
      helpinsub='lib'  ! set for subroutine
      helptopic='invoke_proforma'
      call gethelptext(helpinsub,helptopic,nbhelp)

C There are several location options to present to the user
C depending on whether we are working in the model folder or
C remotely, including the user's home folder. Also take into
C account whether we are browsing the model or own it.
  11  if(browse)then
        LNU=max(1,lnblnk(upath))
        if(cfgroot(1:2).eq.'  ')then
          LN=max(1,lnblnk(uname))
          write(sstr,'(4a)') upath(1:LNU),fs,uname(1:LN),'.gif'
        else
          LN=max(1,lnblnk(cfgroot))
          write(sstr,'(4a)') upath(1:LNU),fs,cfgroot(1:LN),'.gif'
        endif
        call edisp(iuout,'  ')  ! echo a black line
      else
        iw=0
        if(path.ne.'./'.and.path.ne.' ')then
          call edisp(iuout,'  ')  ! echo a black line
          write(outs,'(A,A)') 'The current path is: ',path
          call edisp(iuout,outs)
          CALL EASKABC('You are working in a remote model folder.',
     &      'Options:','use remote model folder','use local folder',
     &      'cancel',IW,nbhelp)
          if(iw.eq.1)then
            LN=max(1,lnblnk(cfgroot))
            write(sstr,'(4a)') path(1:lnblnk(path)),fs,
     &        cfgroot(1:LN),'.gif'
          elseif(iw.eq.2)then
            LN=max(1,lnblnk(cfgroot))
            write(sstr,'(3a)') './',cfgroot(1:LN),'.gif'
          elseif(iw.eq.3)then
            return
          endif
        else
          LN=max(1,lnblnk(cfgroot))
          write(sstr,'(3a)') './',cfgroot(1:LN),'.gif'
        endif
      endif

C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path. sstr is the suggested name and
C lltmp is the file name returned by the user selection.
      lltmp='  '
      CALL EASKXORGTKF(sstr,'Export image file name?',' ',
     &    'currentview.gif',lltmp,'image file',IER,nbhelp)

C If user requested jump back, re-display the menu.
      if(ier.eq.-3)then
        return  ! cancel detected, return.
      endif
      if(ier.ne.0.or.lltmp(1:2).eq.'  ')then
        goto 11
      endif

      ltf=max(1,LNBLNK(lltmp))
      LN=max(1,lnblnk(gprcmd))
      write(gcmd,'(3a)') gprcmd(1:LN),' ',lltmp(1:ltf)

C Invoke the application named in gprcmd with the file name.

C << todo figure out how to determine if gprcmd is installed >>

      call runit(gcmd,'-')

      return

      end

C ******************** PHELPD ********************
C Displays the current contents of common pophelp in a form
C appropriate to the current terminal type. MSG is a short descriptive
C string for the subject, NHELP is the number of lines to be displayed
C and IBX & IBY are the preferred coordinates of the lower left corner
C if for terminal type 8.

      SUBROUTINE PHELPD(MSG,NHELP,OTHER,IBX,IBY,IER)
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/OUTIN/IUOUT,IUIN,IEOUT

C dh is duplicate help list.
      dimension dh(60),lh(36)
      CHARACTER*(*) MSG,OTHER
      CHARACTER outs*73,dh*72,lh*72
      character IA*3
      integer IBX1,IBY1

C Set coordinates IBX and IBY to zero (so parameter not needed).
      IBX1=0; IBY1=0
      IPM = 1
      helpinsub='lib'
      helptopic='phelpd'
      write(outs,'(3a)') helpinsub,' ',helptopic

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
          call dupphelp(dh)
          call updhelp(dh,1,IHW)
          ipflg=0
          CALL egphelp(IBX1,IBY1,IPFLG,0,iuresp)
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
        CALL egphelp(IBX1,IBY1,IPFLG,0,iuresp)
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
             READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1,END=666)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
          elseif(IPM.eq.MPM)then
            call edisp(iuout,
     &        ' Options:  a) previous, b) -, c) continue ')
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1,END=666)IA
            if(IA(1:1).eq.'a'.or.IA(1:1).eq.'A')iw1=1
            if(IA(1:1).eq.'b'.or.IA(1:1).eq.'B')iw1=2
            if(IA(1:1).eq.'c'.or.IA(1:1).eq.'C')iw1=3
          else
            call edisp(iuout,
     &        ' Options:  a) previous, b) next, c) continue ')
            READ(IUIN,'(A3)',IOSTAT=IOS,ERR=1,END=666)IA
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
      
C If an EOF is detected when reading from IUIN, we often get infinite
C looping, creating infinite output. Stop the program.
  666 write(IUOUT,*)'EHELPD: EOF detected, error in input commands.'
      call pauses(1)
      STOP

      END

C ******************** HPAGE ********************
C Controls paging of pop-up help.

      SUBROUTINE HPAGE(act,ILEN,MIFULL,MFULL,IST,IPM,MPM,IPFLG)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      character*(*) act

      if(act(1:6).eq.'create')then
        IST=1
        MFULL=LIMTTY
        MCTL = 2
        MIFULL=MFULL-MCTL
        IF(ILEN.LE.MIFULL)THEN
          IPFLG=0
          IPM = 1
          MPM = 1
        ELSE
          IPFLG=1
          if(MIFULL.eq.0)then
            PAGE=1.0
          else
            PAGE=(FLOAT(IST+MIFULL-1)/FLOAT(MIFULL))
          endif
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

C ******************** dupphelp ********************
C Copies current pop-up help common into a string array so
C that C code does not destroy it.

      SUBROUTINE dupphelp(dh)
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      dimension dh(60)
      CHARACTER dh*72,outs*124

      helpinsub='lib'
      helptopic='duphelp'
      write(outs,'(3a)') helpinsub,' ',helptopic

      DO 50 I=1,60
        last=max(LNBLNK(h(I)),1)
        WRITE(dh(i),'(A)',iostat=ios,err=1)h(I)(1:last)
   50 CONTINUE
      RETURN
   1  write(outs,*) 'dupphelp: error copying help line. '
      call edisp(iuout,outs)
      return
      END

C ******************** PHELPW ********************
C Returns the width (IWH) of the longest text string in the common
C pophelp where IL is the number of lines of help under consideration.

      SUBROUTINE PHELPW(IL,IHW,IER)
#include "help.h"
      integer ill   ! local loop
      character outs*124
  
      helpinsub='lib'       ! currently not used
      helptopic='duphelp'
      write(outs,'(3a)') helpinsub,' ',helptopic

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


C ******************** EVSET ********************
C Provides setup environment for wire frame view w/in prj.
C IER=0 OK, IER=1 problem. If izgfoc is not 0 then it
C represents the zone that is currently being edited.

      SUBROUTINE EVSET(act,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "espriou.h"
#include "help.h"

      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/rpath/path
      common/appw/iappw,iappx,iappy
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

C The RAY2 common block variables from prj3dv.h are:
C ITDSP toggle: all surf+obs+vis = 0, all surf = 1, ext = 2,
C partn = 3, similar = 4, surfs+obs+ground = 5, ground only = 6
C surf+obs = 7, surf+vis = 8
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
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,CFCDBOK
      LOGICAL OPTKOK,CLOSE,OK,found,concat,do_redraw
      DIMENSION ITEMS(24),IVALS(MCOM),FALT(9),IVAL(9)
      CHARACTER ITEMS*32,LFIL*72,outs*124
      DIMENSION STMP(MS)
      character STMP*46
      CHARACTER FALT*37
      CHARACTER path*72
      character doit*248,tmode*8,longtfile*144, hold*24,act*1
      CHARACTER head*19,msg*96,defcog*24,msg2*32
      logical modmlc  ! for selecting MLC
C      integer lastifs,lastitfs,lastimfs

      integer IW  ! for radio buttons
      integer MVERT,IVERT  ! max items and current menu item

#ifdef OSI
      integer iappr,iappt,iappb ! for use with findrtb
      integer iapprp,iapptp,iappbp ! for local offsets
#else
      integer*8 iappr,iappt,iappb ! for use with findrtb
      integer*8 iapprp,iapptp,iappbp ! for local offsets
#endif

      helpinsub='lib'

C If not in graphic mode return.
      if(act.eq.'-')then
        continue
      else
        if(MMOD.lt.8)return
      endif

C Initial menu entry setup with one line header, 3 control lines.
      do_redraw=.false.
      ITPPSW=0

C Set image control menu font sizes (IMFS is for menus, IFS is for dialog & ITFS text feedback).
C Fonts 4-7 are proportional and 0-3 are fixed width. Use smallest proportional for menus
C and dialog. Because it impacts interpretation of the width of the graphic feedback area
C comment out this block of code as well as the restoring code near line 4775.
C      lastimfs=IMFS; IMFS=4
C      lastifs=IFS; IFS=4
C      lastitfs=ITFS; ITFS=4
C      call userfonts(IFS,ITFS,IMFS)

C Menu loop.
   92 IER=0
      IVERT=-3

      WRITE(ITEMS(1),'(A,3F6.1)')'a eye point:',(EYEM(J),J=1,3)
      WRITE(ITEMS(2),'(A,3F6.1)')'b view point:',(VIEWM(J),J=1,3)
      WRITE(ITEMS(3),'(A,F6.1)') 'c angle of view:',ANG
      if(ITPPSW.eq.0)then
        ITEMS(4) ='d view >> perspective     '
      elseif(ITPPSW.eq.1)then
        ITEMS(4) ='d view >> plan view       '
      elseif(ITPPSW.eq.2)then
        ITEMS(4) ='d view >> south elevation '
      elseif(ITPPSW.eq.3)then
        ITEMS(4) ='d view >> west elevation  '
      endif
      ITEMS(5)   =' _____________________________  '

      IF(ITDSP.EQ.0)THEN
        ITEMS(6) ='e display >> surfaces+obstr+vis '
      ELSEIF(ITDSP.EQ.1)THEN
        ITEMS(6) ='e display >> all surfaces       '
      ELSEIF(ITDSP.EQ.2)THEN
        ITEMS(6) ='e display >> exterior surfaces  '
      ELSEIF(ITDSP.EQ.3)THEN
        ITEMS(6) ='e display >> partitions+int surf'
      ELSEIF(ITDSP.EQ.4)THEN
        ITEMS(6) ='e display >> const.boundary surf'
      ELSEIF(ITDSP.EQ.5)THEN
        ITEMS(6) ='e display >> surfs+obstr+ground '
      ELSEIF(ITDSP.EQ.6)THEN
        ITEMS(6) ='e display >> ground only        '
      ELSEIF(ITDSP.EQ.7)THEN
        ITEMS(6) ='e display >> surfs+obstructions '
      ELSEIF(ITDSP.EQ.8)THEN
        ITEMS(6) ='e display >> surfs+visible enty '
      ENDIF

C Highlight surface based on OPQ/TRAN or construction.
      IF(ITHLS.EQ.0)THEN
        ITEMS(7) ='f highlight >> normal           '
      ELSEIF(ITHLS.EQ.1)THEN
        if(ITHLZ.gt.0)then
          WRITE(ITEMS(7),'(2A)')'f highlight >> ',
     &      mlcname(ITHLZ)(1:16)
        else
          ITEMS(7) ='f highlight >> composition      '
        endif
      ELSEIF(ITHLS.EQ.2)THEN
        ITEMS(7) ='f highlight >> transparent/opaq '
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
        ITEMS(15)='m grid distance    >> optimum   '
      ELSE
        WRITE(ITEMS(15),23)GRDIS
  23    FORMAT('m grid distance:   >>',F5.2,' (m)')
      ENDIF

      IF(ITSNR.EQ.0)THEN
        ITEMS(16)='n surface normals  >> display   '
      ELSEIF(ITSNR.EQ.1)THEN
        ITEMS(16)='n surface normals  >> hidden    '
      ENDIF

      ITEMS(17)  =' _____________________________  '

      ITEMS(18)  ='* zones in image                '
      ITEMS(19)  ='! refresh image                 '
      ITEMS(20)  ='1 hidden line view              '
      ITEMS(21)  ='2 views from sun                '
      ITEMS(22)  ='* mouse controls                '
      ITEMS(23)  ='? help                          '
      ITEMS(24)  ='- exit menu                     '
      MVERT=24

C If image info altered (view point) then update display.
      IF(MODIFYVIEW)THEN
        if(ITPPSW.eq.0)then
          CALL INLNST(1)
          CALL redraw(IER)
          IF(ier.NE.0)RETURN
          MODLEN=.FALSE.
          MODBND=.FALSE.
          MODIFYVIEW=.FALSE.
        elseif(ITPPSW.eq.1)then
          if(do_redraw)call PLELEV('P')
        elseif(ITPPSW.eq.2)then
          if(do_redraw)call PLELEV('S')
        elseif(ITPPSW.eq.3)then
          if(do_redraw)call PLELEV('E')
        endif
      ENDIF

C Display the menu. If in response to image control button
C then shift its position slighty (taking into account the current
C width of the control menu.
      if(act.eq.'-')then
        head='Viewing environment'
        CALL EMENU(head,ITEMS,MVERT,IVERT)
      elseif(act.eq.'B')then
        call findrtb(iappr,iappt,iappb)
        head='Image control'
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

C Uncomment if we want a small font and need to
C restore standard sizes on exit.
C        IMFS=lastimfs  ! restore values from entry point
C        IFS=lastifs
C        ITFS=lastitfs
C        call userfonts(IFS,ITFS,IMFS)

        RETURN
      ELSEIF(IVERT.EQ.1)THEN
        H(1)='The eye point is relative to the site coordinate system.'
  43    WRITE(HOLD,'(1x,3f7.1)')EYEM(1),EYEM(2),EYEM(3)
        CALL EASKS(HOLD,' ','Eye point X Y Z?',
     &     24,' -100. -100. 100. ','eye point coord',IER,1)
        K=0
        CALL EGETWR(HOLD,K,EYEM(1),-999.,999.,'W','X cord',IER)
        CALL EGETWR(HOLD,K,EYEM(2),-999.,999.,'W','Y cord',IER)
        CALL EGETWR(HOLD,K,EYEM(3),0.,999.,'W','eye Z cord',IER)
        if(ier.ne.0)goto 43
        MODLEN=.TRUE.
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.2)THEN

C Present viewpoint coords for editing then parse data from HOLD.
        call eclose(XMN,0.0,0.001,CLOSE)
        if(XMN.LT.0..AND.XMX.GE.0.0)then
          COGXM=XMN+((XMX+ABS(XMN))/2.0)
        elseif(XMN.LT.0.0.AND.XMX.LE.0.)then
          COGXM=XMN+((ABS(XMN)-ABS(XMX))/2.0)
        elseif(XMN.GT.0.0.AND.XMX.GT.0.0)then
          COGXM=XMX-((XMX-XMN)/2.0)
        elseif(CLOSE.AND.XMX.GT.0.0)then
          COGXM=XMX/2.0
        endif
        call eclose(YMN,0.0,0.001,CLOSE)
        if(YMN.LT.0..AND.YMX.GE.0.0)then
          COGYM=YMN+((YMX+ABS(YMN))/2.0)
        elseif(YMN.LT.0.0.AND.YMX.LE.0.)then
          COGYM=YMN+((ABS(YMN)-ABS(YMX))/2.0)
        elseif(YMN.GT.0.0.AND.YMX.GT.0.0)then
          COGYM=YMX-((YMX-YMN)/2.0)
        elseif(CLOSE.AND.YMX.GT.0.0)then
          COGYM=YMX/2.0
        endif
        call eclose(ZMN,0.0,0.001,CLOSE)
        if(ZMN.LT.0.0.AND.ZMX.GE.0.0)then
          COGZM=ZMN+((ZMX+ABS(ZMN))/2.0)
        elseif(ZMN.LT.0.0.AND.ZMX.LE.0.0)then
          COGZM=ZMN+((ABS(ZMN)-ABS(ZMX))/2.0)
        elseif(ZMN.GT.0.0.AND.ZMX.GT.0.0)then
          COGZM=ZMX-((ZMX-ZMN)/2.0)
        elseif(CLOSE.AND.ZMX.GT.0.0)then
          COGZM=ZMX/2.0
        endif
        write(msg,'(a,3f7.2)') 'Model COG is ',COGXM,COGYM,COGZM
        write(defcog,'(3f7.2)') COGXM,COGYM,COGZM
        call edisp(iuout,msg)

C Offer default as COG of model or current zone or current
C surface or at least display what these values are
        H(1)='The view point is a point within the site coordinate'
        H(2)='system. The default is the model centre of gravity.'
  42    WRITE(HOLD,'(1x,3f7.1)')VIEWM(1),VIEWM(2),VIEWM(3)
        CALL EASKS2CMD(HOLD,' ','View point X Y Z?',
     &    'zone COG','surface COG',iclkok,24,defcog,
     &    'viewpoint coord',IER,2)
        if(iclkok.eq.0)then
          K=0
          CALL EGETWR(HOLD,K,VIEWM(1),-999.,999.,'W','X cord',IER)
          CALL EGETWR(HOLD,K,VIEWM(2),-999.,999.,'W','Y cord',IER)
          CALL EGETWR(HOLD,K,VIEWM(3),0.,999.,'W','view Z cord',IER)
        elseif(iclkok.eq.1)then

C Select one of the current zones.
          H(1)='Pick one zone for view focus.'
          INPIC=1
          CALL EPICKS(INPIC,IVALS,' ','Zone(s) focus:',
     &      12,NCOMP,zname,'zone list',IER,1)
          write(msg,'(a,3f7.2)') 'Zone COG is ',ZCOG(IVALS(1),1),
     &      ZCOG(IVALS(1),2),ZCOG(IVALS(1),3)
          call edisp(iuout,msg)
          VIEWM(1)=ZCOG(IVALS(1),1)  ! set view to zone COG
          VIEWM(2)=ZCOG(IVALS(1),2)
          VIEWM(3)=ZCOG(IVALS(1),3)
        elseif(iclkok.eq.2)then

C Select one of the current zones and then a surface.
          H(1)='Pick one zone for view focus.'
          INPIC=1
          CALL EPICKS(INPIC,IVALS,' ','Zone(s) focus:',
     &      12,NCOMP,zname,'zone list',IER,1)
          izone=IVALS(1)
          DO 11 I=1,NZSUR(IZONE)
           lnl=lnblnk(SMLCN(izone,i))
           if(lnl.gt.16) lnl=16  ! truncate MLC name in list
           write(STMP(I),'(5a)') SNAME(izone,i),'|',
     &       SMLCN(izone,i)(1:lnl),
     &       '|',SOTHER(izone,i,1)(1:16)
   11     CONTINUE
          INPIC=1
          write(msg2,'(a)') ' name & type & exposure'
          CALL EPICKS(INPIC,IVALS,' ','Surface(s) focus:',
     &      33,NZSUR(IZONE),STMP,msg2,IER,nbhelp)
          isur=IVALS(1)
          write(msg,'(a,3f7.2)') 'Surface COG is ',SURCOG(izone,isur,1),
     &      SURCOG(izone,isur,2),SURCOG(izone,isur,3)
          call edisp(iuout,msg)
          VIEWM(1)=SURCOG(izone,isur,1)  ! set view to surface COG
          VIEWM(2)=SURCOG(izone,isur,2)
          VIEWM(3)=SURCOG(izone,isur,3)
        endif
        if(ier.ne.0)goto 42
        MODLEN=.TRUE.
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.3)THEN
        H(1)='Specify an angle of view between 1 and 89 degrees.'
        CALL EASKR(ANG,' ','Angle of view?',
     &             1.0,'W',89.0,'W',40.,'angle ofview',IER,1)
        HANG=ANG/2.0
        MODLEN=.TRUE.
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.4)THEN
        H(1)='In plan view you can change the viewing parameters;'
        H(2)='the elevations are primarily for model checking.'
        IW=1
        CALL EASKABCD(' ','View options:','perspective',
     &    'plan','south elevation','west elevation',IW,2)
        if(IW-1.ne.ITPPSW)then
          do_redraw =.true.
          MODIFYVIEW=.TRUE.
        endif
        ITPPSW=IW-1
      ELSEIF(IVERT.EQ.6)THEN

C If configuration file loaded then can filter surfaces by location.
C Allow this selection by text string.
        IF(CFGOK)THEN
          H(1)='The image may be filtered to focus on specific aspects'
          H(2)='of the model. Current filters are listed.'
          FALT(1)='all surfaces + obstr + visual entity '
          FALT(2)='all surfaces (no obstructions)       '
          FALT(3)='exterior surfaces only               '
          FALT(4)='partitions & interior surfaces only  '
          FALT(5)='surf. connected to ground or constant'
          FALT(6)='all surfaces, obstructions & ground  '
          FALT(7)='ground topology only                 '
          FALT(8)='all surfaces & shading obstructions  '
          FALT(9)='all surfaces & visual entities       '
          IV=1
          CALL EPICKS(IV,IVAL,' ','Image filter:',
     &      37,9,FALT,' image filters',IER,2)
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
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.7)THEN

C Highlight control.
        h(1)='Display of surfaces can be limited to surfaces which'
        h(1)='match the given filter. '
        IW=1
        CALL EASKATOG(' ','Highlight options:','normal','composition',
     &    'opaque/transp.','partial attrib.','cancel',' ',' ',IW,2)
        ITHLS=IW-1
        if(ITHLS.eq.0)then
          MODIFYVIEW=.true.
        elseif(ITHLS.eq.1)then
          if(mlcver.eq.0)then
            CALL EPKMLC(ISEL,'Select a construction to highlight.',
     &        ' ',IER)
          else
            call edisp(iuout,'Select a construction to highlight.')
            CALL EDMLDB2(modmlc,'-',ISEL,IER)
          endif
          ITHLZ=ISEL
          MODIFYVIEW=.TRUE.
        elseif(ITHLS.eq.2)then
          CALL EASKABC(' ','Options:','highlight opaque',
     &    'highlight transparent','cancel',IW,2)
          ITHLZ=IW
          MODIFYVIEW=.TRUE.
        elseif(ITHLS.eq.3)then
          MODIFYVIEW=.TRUE.
        endif
      ELSEIF(IVERT.EQ.8)THEN

C So what does ITBND=3 (focus on zone) actually mean?
        ITBND=ITBND+1
        IF(ITBND.GT.1)ITBND=0
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.10)THEN
        ITZNM=ITZNM+1
        IF(ITZNM.GT.1)ITZNM=0
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.11)THEN
        ITSNM=ITSNM+1
        IF(ITSNM.GT.1)ITSNM=0
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.12)THEN
        ITVNO=ITVNO+1
        IF(ITVNO.GT.1)ITVNO=0
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.13)THEN

C Site origin, see if w/in bounds to flag bounds and lens...
        ITORG=ITORG+1
        IF(ITORG.GT.1)ITORG=0
        IF(1.1.GT.XMX.OR.1.1.LT.XMN) MODBND=.TRUE.
        IF(1.1.GT.YMX.OR.1.1.LT.YMN) MODBND=.TRUE.
        IF(0.0.GT.ZMX.OR.0.0.LT.ZMN) MODBND=.TRUE.
        if(MODBND)then
          MODLEN=.TRUE.
          MODIFYVIEW=.TRUE.
        endif
      ELSEIF(IVERT.EQ.14)THEN
        ITGRD=ITGRD+1
        IF(ITGRD.GT.1)ITGRD=0
        MODBND=.TRUE.
        MODLEN=.TRUE.
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.15)THEN
        H(1)='The distance between the site grid lines can be'
        H(2)='specified. If set to 0 then an optimum value will'
        H(4)='be determined.'
        CALL EASKR(GRDIS,' ','Grid line distance?',
     &             0.0,'F',10.0,'W',0.,'grid distanc e',IER,3)
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.16)THEN
        ITSNR=ITSNR+1
        IF(ITSNR.GT.1)ITSNR=0
        MODIFYVIEW=.TRUE.
      ELSEIF(IVERT.EQ.18)THEN

C Select zones to include, if one zone is the principal focus then
C make sure it is included in the list.
        IF(CFGOK)THEN
          H(1)='Select the zones to include in the image.'
          INPIC=NCOMP

C Test with groups as well via local version of ASKMULTIZON.
          call ASKMULTIZON(INPIC,IVALS,'Zones to include:',
     &      'zone list','-',IER) 
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
          MODIFYVIEW=.TRUE.
        ELSE
          CALL USRMSG(' ','Model not yet defined!','W')
          RETURN
        ENDIF
      ELSEIF(IVERT.EQ.19.AND.INPIC.GE.1)THEN

C Refresh the image.
        MODLEN=.TRUE.
        MODBND=.TRUE.
        MODIFYVIEW=.TRUE.
        do 444,ix=1,ncomp
          iZBFLG(ix)=0
  444   continue
        CALL redraw(IER)
        IF(ier.NE.0)RETURN
        MODLEN=.FALSE.
        MODBND=.FALSE.
        MODIFYVIEW=.FALSE.
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
     &    'Options:','use remote folder','use home folder',IW,0)
        endif

C Get file name.
        H(1)='This file will contain the model topography and'
        H(2)='topology in a format that can be passes to the'
        H(3)='VIEWER hidden line tool.'
        CALL EASKS(LFIL,' ','Model file?',
     &              72,' ','perspective input file',IER,3)
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
     &          'Problem creating model file!','W')
          goto 92
        ENDIF
        h(1)='Be sure to tidy up any *.vew files that might'
        h(2)='have been automatically created.'
        CALL EASKOK(' ','Display perspective views?',OK,2)
        IF(OK)then

C Get logical name of child process terminal type, expand model
C name to include the path (if applicable) and create a string to
C drive viewer.  No point in using viewer in text mode so force
C tmode as 'graphic'
          doit = ' '
          tmode='graph'
          if(iw.eq.1)then
            call addpath(LFIL,longtfile,concat)
          else
            write(longtfile,'(a)') LFIL(1:lnblnk(LFIL))
          endif
          if(iappw.gt.0.and.iappw.le.200)then
            write(doit,'(3a,3i4,3a)') 'viewer -mode ',tmode,
     &        ' -s ',iappw,iappx+25,iappy+20,' -file ',
     &        longtfile(1:lnblnk(longtfile)),' & '
          else
            write(doit,'(5a)') 'viewer -mode ',tmode,
     &        ' -s 0 0 0 -file ',longtfile(1:lnblnk(longtfile)),' & '
          endif
          call runit(doit,tmode)
        endif
      ELSEIF(IVERT.EQ.21)THEN

C Draw the zones and any obstructions based on sun position
C at user specified time (offer 11 am on day of average solar
C declination in June). Use chgsun subroutine in cfommon3dv.F
        call edisp(iuout,'Setup initial date and time for solar view.')
        call chgsun(0)

C List mouse controls in the view window.
      ELSEIF(IVERT.EQ.22)THEN
        helptopic='mouse_controls'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('mouse controls',nbhelp,'-',0,0,IER)

C Help text.
      ELSEIF(IVERT.EQ.23)THEN
        helptopic='lib_general'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('3dv menu',nbhelp,'-',0,0,IER)
      ELSE

C Not one of the legal menu choices.
        IVERT=-1
        GOTO 92
      ENDIF
      IVERT=-2
      GOTO 92

      END

C ******************** ASKMULTIZON ********************
C Presents a list of zones to select one or more from.
C It is passed a prompt, menu title, and number of allowable items inpic
C It returns inpick and array IVALS. Variant of standard version in
C esru_misc.F 
C MOD display modifier `-` zone names only.
C If there have been groups-of-zones defined also include these
C in the list (at the bottom).

      SUBROUTINE ASKMULTIZON(INPIC,IVALS,prompt,title,mod,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "schedule.h"
#include "epara.h"
#include "help.h"

      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      LOGICAL FOUND
      CHARACTER*(*) prompt,title

      DIMENSION VERT(35),IVALS(MCOM)
      DIMENSION GERT(32),IGVAL(32)  ! For groups.
      CHARACTER VERT*23,gert*23,KEY*1,prompt2*36
      character mod*1,outs*124,outsd*124,msg*96

C Clear IVALS.
      IER=0
      maxinpic=INPIC; INPIC=0
      DO 40 I=1,NCOMP
        IVALS(I)=0
   40 CONTINUE
      helpinsub='lib'
      helptopic='askmultizone'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Makeup list of zone groups.
      if(nzgroup.gt.0)then
        do i=1,nzgroup
          write(gert(i),'(2a,i2,a)')zglbl(i)(1:lnblnk(zglbl(i))),' (',
     &     izgnumber(i),'zn)'
        enddo
      endif

C Initialise zone menu variables based on window size.
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=0
      if(nzgroup.gt.0)then
        MCTL=5
      else
        MCTL=4
      endif
      ILEN=NCOMP
      IPACT=CREATE
      CALL EKPAGE(IPACT)

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Build up text strings for the menu.
    3 M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          VERT(M)=' '
          WRITE(VERT(M),'(a1,1x,a)')KEY,zname(L)
          do K=1,INPIC  ! mark those selected
            if (IVALS(K).eq.L) then
              WRITE(VERT(M),'(a1,1x,2a)')KEY,zname(L),' *'
            endif
          enddo
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text and info on portion seen.
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=  '  __________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM
   15   FORMAT ('0 page part: ',I1,' -- of:',I1)
      ENDIF

C If groups defined.
      if(nzgroup.gt.0)then
        if(maxinpic.eq.NCOMP)then
          VERT(M+2)='* All                 '
        else
          VERT(M+2)='                      '
        endif
        VERT(M+3)  ='! via group-of-zones  '
        VERT(M+4)  ='? help                '
        VERT(M+5)  ='- exit menu'
      else
        if(maxinpic.eq.NCOMP)then
          VERT(M+2)='* All                 '
        else
          VERT(M+2)='                      '
        endif
        VERT(M+3)  ='? help                '
        VERT(M+4)  ='- exit menu'
      endif
      write(prompt2,'(a)') ' select one or more...'

C Display the menu.
      call usrmsg(PROMPT,PROMPT2,'-')
      CALL EMENU(title,VERT,MVERT,IVERT)
      IF(IVERT.LE.MHEAD)THEN
        IVERT=-1
        goto 3
      ELSEIF(IVERT.EQ.MVERT)THEN

        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Display help text.
        CALL PHELPD('zone files section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2).AND.nzgroup.gt.0)THEN

C Display a list of groups of associated zones.
        INPIC=1
        CALL EPMENSV
        CALL EPICKS(INPIC,IGVAL,' ','Available groups:',
     &    23,nzgroup,gert,'group list',IER,nbhelp)
        CALL EPMENRC
        write(msg,'(2a,i2,a)') 
     &    zglbl(IGVAL(1))(1:lnblnk(zglbl(IGVAL(1)))),' includes ',
     &    izgnumber(IGVAL(1)),' zones:'
        call edisp(iuout,msg)
        if(IGVAL(1).gt.0)then

C Convert into a single string similar to logic used
C in eres/utils.F ZNLIST.
          INPIC=0; limit=izgnumber(IGVAL(1))
          length=0
          do i=1,limit
            lna=lnzname(izglist(IGVAL(1),i))
            length=length+lna+1
            INPIC=INPIC+1       ! increment counter
            IVALS(INPIC)=izglist(IGVAL(1),i)
            write(outs,*) 'including ',zname(IVALS(INPIC))
            call edisp(iuout,outs)
          enddo
          if(length.lt.124)then
            WRITE(outs,5,iostat=ios,err=1)
     &        (zname(izglist(IGVAL(1),i)),i=1,limit)
    5       FORMAT(' Zones: ',20(a,' '))
            call sdelim(outs,outsd,'S',IW)
            call edisp(iuout,outsd)
            return

C I/O error trap.
   1        return
          else
            continue
          endif
          return
        endif
      ELSEIF(IVERT.EQ.(MVERT-3).AND.nzgroup.gt.0)THEN
        DO I=1,NCOMP  ! Select all zones.
          IVALS(I)=I
        ENDDO
        INPIC=NCOMP
        call edisp(iuout,'All zones selected')
        return
      ELSEIF(IVERT.EQ.(MVERT-2).AND.nzgroup.eq.0)THEN
        DO I=1,NCOMP  ! Select all zones.
          IVALS(I)=I
        ENDDO
        INPIC=NCOMP
        call edisp(iuout,'All zones selected')
        return
      ELSEIF(IVERT.EQ.(MVERT-4).AND.nzgroup.gt.0)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          IVERT=-2
          goto 3
        ENDIF
      ELSEIF(IVERT.EQ.(MVERT-3).AND.nzgroup.eq.0)THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          IVERT=-2
          goto 3
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Look through previous selections and see if IFOC is unique, if
C so update IVALS and loop back for another.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPIC.GT.0)THEN
          DO 44 J=1,INPIC
            IF(IVALS(J).EQ.IFOC.or.FOUND) then
              FOUND=.TRUE.
              if (J+1.gt.NCOMP) then
                IVALS(J)=0
              else
                IVALS(J)=IVALS(J+1)
              endif
            endif
  44      CONTINUE
          IF(.NOT.FOUND)THEN
            if (INPIC.lt.NCOMP) then
              INPIC=INPIC+1
              IVALS(INPIC)=IFOC
              write(outs,*) 'added ',vert(ivert)(2:lnblnk(vert(ivert)))
              call edisp(iuout,outs)
              goto 3
            endif
          ELSE
            INPIC=INPIC-1
            write(outs,*) 'removed ',
     &        vert(ivert)(2:lnblnk(vert(ivert)))
            call edisp(iuout,outs)
            goto 3  ! to re-fresh the items in the list.
          ENDIF
        ELSEIF(INPIC.EQ.0)THEN
          INPIC=1
          IVALS(INPIC)=IFOC
          write(outs,*) 'added ',vert(ivert)(2:lnblnk(vert(ivert)))
          call edisp(iuout,outs)
          goto 3
        ENDIF
      endif

      IF(IVERT.EQ.(MVERT-3))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      else

C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      endif
      IVERT=-2
      goto 3

      END


C ******************** startbuffer ********************
C Wrapper around call to win3dclr in esru_x.c to match
C the functionality of startbuffer in the GTK version.

       subroutine startbuffer()
       call win3dclr
       return
       end

C ******************** popupimage ********************
C Display image with documentation (dummy of GTK version).

      subroutine popupimage(head,topic,act,longtfile)
      character*(*) head,topic,act,longtfile
      return
      end

