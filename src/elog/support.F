C Support code for bamstats - copied from lib folder.
C From esru_lib.F
C  EGETW:  Finds first word after pos k in a string (' ' ',' '|' or tab separated).
C  EGETP:  Finds first phrase after pos k in a string (delimeter separated).
C  EGETWI: As EGETW for an integer with range checking & error messages.
C  EGETWR: As EGETW for a real with range checking & error messages.
C EDISP248 Displays a 248 char block of text (text or graphic).
C  iprevblnk: Given a string, return position of blank just before ipos.
C  inextblnk: Given a string, return position of blank just after ipos.
C  isunix:  Checks if machine type is Unix or NT.
C  INTSTR: Converts integer into string (10 char) w/o leading blanks.
C  RELSTR: Converts a real into a string (12 char) w/o leading blanks.
C  REL16STR: Converts a real into a string (16 char) w/o leading blanks.
C  ARLIST: takes a real array (rlist) and builds a packed string.
C  ECLOSE:  Checks tolerance between two real numbers.
C  EDAY:   Returns the year day number when passed the day of month & month.
C  EDAYR: returns the day and month numbers from the day-of-year.
C  EFOPSEQ: Open a sequential file with existance flag & path check.
C  ERPFREE: Is used to close any file.
C  EFDELET: Delete the current file opened under IUN and return ISTAT.
C  C2FSTR:  Convert c function returned string to fortran format.
C  EPAGE:   Screen control: page without waiting.
C  EPAGEW:  Screen control: Wait before paging.
C  EPWAIT:  Screen control: Wait without paging.
C  EPAGEND: Screen control: Page then close window if open.
C  ADDPATH: Return file name appended onto the path and logical concat.
C  tstamp:  Date stamp with message.
C  dstamp:  Get date stamp in the form: Fri Jan 23 09:34:31 1998.
C  STRIPC1K strips comments from a ASCII file (1000 char long) string and returns the data.
C  iCountWords: Counts the number of space/tab/comma-separated
C           words in a string.
C  SDELIM:  Replaces blanks in a string A with alternative delimiter.
C clrtextbuf: Clears the graphic text buffer common blocks.

C From esru_libNonGTK.F
C  USRMSG:  Generic message/prompt facility for all terminal types.
C  LUSRMSG:  Generic long message/prompt facility for all terminal types.
C  EDISP:   Generic send text to scrolling display (text or graphic).
C  PHELPD:  Displays the current contents of common pophelp.
C  EMPAGE:  Low level screen control for paging based on terminal model.
C  EPAGES:  Initialise terminal, set up a scratch pad & line count.
C  ASKOK:   Generic yes/no/default facility returning OK as a logical parameter.
C  EASKS:   Ask user for a string with prompt, error messages & help.
C  PHELPW:  Returns the width IWH of the longest popup help string.

C From esru_ask.F
C   helpwithblank(ermsg,nbhelp,newnbhelp,ier)


C ***************** EGETW
C EGETW gets first WORD after position K from the STRING of
C characters. Words are separated by blanks, commas, |, or tab: WORD,WORD,WORD
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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*(*) WORD, STRING, MSG
      CHARACTER ACT*1,A*1,loutstr*248,outs*124
      logical unixok

      WORD=' '
      LS=LEN(STRING)
      LW=LEN(WORD)
      L=0

C Start by skipping blanks and tabs before the word.
   10 K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9).OR.
     &   (ICHAR(A).eq.124)) GOTO 10

C Copy WORD from STRING, character by character until separator found.
   20 L=L+1
      IF(L.GT.LW) GOTO 100
      WORD(L:L)=A
      K=K+1
      IF(K.GT.LS) GOTO 999
      A=STRING(K:K)
      IF(A.EQ.' '.OR.A.EQ.','.OR.(ICHAR(A).EQ.9).OR.
     &   (ICHAR(A).eq.124)) GO TO 100
      GOTO 20

  100 RETURN

  999 ier=1
      if(ACT.EQ.'-')then
        RETURN
      elseif(ACT.EQ.'W')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (word) in...'
        endif
      elseif(ACT.EQ.'F')then
        ier=1
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (word) in...'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETW: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETW: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END

C ***************** EGETP
C EGETP gets first PHRASE after position K from the STRING of
C characters. Phrases are separated by tabs or commas. Provides a warning
C message if ACT='W', a failure message if ACT='F' and does
C no message if ACT='-'.
      SUBROUTINE EGETP(STRING,K,PHRASE,ACT,MSG,ier)
#include "espriou.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*(*) PHRASE, STRING, MSG
      CHARACTER ACT*1,A*1,loutstr*248,outs*124
      logical unixok

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
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'WARNING: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      elseif(ACT.EQ.'F')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        if(currentfile(1:2).ne.'  ')then
          WRITE(loutstr,'(5a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: in ',currentfile(1:LN),
     &      ' past end of line for ',MSG(1:LNM),' (phrase) in...'
        else
          WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &      'FAILURE: Past end of line for ',
     &      MSG(1:LNM),' (phrase) in...'
        endif
      endif
      CALL EDISP248(iuout,loutstr,100)
      if(lnblnk(STRING).gt.123)then
        CALL EDISP248(iuout,STRING,100)
      else
        CALL EDISP(iuout,STRING)
      endif
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETP: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETP: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      CHARACTER*(*) STRING, MSG
      CHARACTER ACT*1,STR1*10,STR2*10,WORD*20,loutstr*248,outs*124
      logical unixok

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
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &      STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') < normal minimum ',STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
        ELSEIF(ACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &        STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') < allowable minimum ',
     &       STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
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
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'WARNING: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' WARNING in: ',
     &        STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') > normal maximum ',STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
        ELSEIF(ACT.EQ.'F')THEN
          call edisp(iuout,' ')
          LN=max(1,lnblnk(currentfile))
          LNM=max(1,lnblnk(MSG))
          if(currentfile(1:2).ne.'  ')then
            WRITE(loutstr,'(4a)',IOSTAT=IOS,ERR=1)
     &        'FAILURE: in ',currentfile(1:LN),': ',STRING(1:ils)
          else
            write(loutstr,'(a,a)',IOSTAT=IOS,ERR=1)' FAILURE in: ',
     &        STRING(1:ils)
          endif
          CALL EDISP248(iuout,loutstr,100)
          write(loutstr,'(7a)',IOSTAT=IOS,ERR=1) ' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > allowable maximum ',
     &       STR2(1:IW2),'!'
          CALL EDISP248(iuout,loutstr,100)
          IER=1
          RETURN
        ENDIF
      ENDIF

  100 RETURN

 1002 if(currentfile(1:2).ne.'  ')then
        LN=max(1,lnblnk(currentfile))
        LNM=max(1,lnblnk(MSG))
        WRITE(loutstr,'(6a)',IOSTAT=IOS,ERR=1) 'Failed in ',
     &    currentfile(1:LN),': conversion of ',
     &    MSG(1:LNM),' value in ',WORD
      else
        LNM=max(1,lnblnk(MSG))
        WRITE(loutstr,1003,IOSTAT=IOS,ERR=1)MSG(1:LNM),WORD
 1003   FORMAT(' Failed: conversion of ',A,' value in ',A)
      endif
      CALL EDISP248(iuout,loutstr,100)
      IER=2
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETWI: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETWI: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C LOUTSTR is for messages to the user and should be long enough
c to prevent truncation of messge contents.
      CHARACTER*(*) STRING, MSG
      CHARACTER RACT*1,STR1*16,STR2*16,WORD*20,LOUTSTR*248,outs*124
      logical unixok

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
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &      ' value (',STR1(1:IW1),') < normal minimum ',STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,100)
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
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') < allowable minumum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,100)
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
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > normal  maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,LOUTSTR,100)
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
          call edisp248(iuout,LOUTSTR,100)
          write(LOUTSTR,'(7a)',IOSTAT=IOS,ERR=1)' the ',MSG(1:LNM),
     &       ' value (',STR1(1:IW1),') > allowable maximum ',
     &       STR2(1:IW2),'!'
          call edisp248(iuout,loutstr,100)
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
      CALL EDISP248(iuout,LOUTSTR,100)
      IER=2
      GOTO 100

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      LNM=max(1,lnblnk(STRING))
      if(IOS.eq.2)then
        write(outs,*) 'EGETWR: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      else
        write(outs,*) 'EGETWR: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,STRING(1:LNM))
      endif
      return

      END

C ********************* EDISP248
C EDISP248 displays a 248 char block of text passed to it
C in a format depending on the terminal type. If it will not
C fit on one line, subsequent lines are used and breaks are
C set based on nearest width to iwid.
      SUBROUTINE EDISP248(ITRU,MSG,iwid)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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

C Debug.
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

C ***************** INTSTR
C INTSTR converts an integer into a string (10 char long) with no
C leading blanks. ISWD is the length of the resulting string.
      SUBROUTINE INTSTR(INTIN,FSTR,ISWD,IFLAG)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

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

C *********************** RELESTR
C REL16STR converts a real into a string (16 char) with no leading
C blanks. ISWD is the actual length of the resulting string. Takes
C the magnitude of the number into account.
      SUBROUTINE RELESTR(RELIN,FSTR,ISWD,IFLAG)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

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
        WRITE(CSTR,'(E11.4)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.1000000.0.and.abs(rval).gt.10000.0)then
        WRITE(CSTR,'(F8.0)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.10000.0.and.abs(rval).gt.100.)then
        WRITE(CSTR,'(f6.0)',IOSTAT=ios,ERR=999)RVAL
      elseif(abs(rval).le.100.0.and.abs(rval).gt.0.001)then
        WRITE(CSTR,'(F7.3)',IOSTAT=ios,ERR=999)RVAL
      else
        WRITE(CSTR,'(E11.4)',IOSTAT=ios,ERR=999)RVAL
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
        WRITE(outs,*) ' RELESTR: permission issue: ',RELIN
      else
        WRITE(outs,*) ' RELESTR: invalid real or > 12 char: ',RELIN
      endif
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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      dimension rlist(inrs)
      character*(*) pckstr
      CHARACTER item*16,delm*1

C      character outs*124  ! for debug statement

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

C The full array could not be packed into the string so return
C the position this happened so calling code can deal with it.
   1  continue

C Debug.
C      write(outs,*) 'ARLIST: truncation writing array at item: ',
C     &  itrunc,' & pos ',length
C      call edisp(iuout,outs)

      return
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

C ******************** EFOPSEQ
C EFOPSEQ: Open a sequential ASCII file (SFILE) and return appropriate
C messages and error status (IEXIST).
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
#include "help.h"

C      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      LOGICAL XST,OK,concat,DOK
      CHARACTER*(*) SFILE
      character outs*144
      CHARACTER LFIL*72
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
          OPEN(IUN,FILE=longtfile,ACCESS='SEQUENTIAL',
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
          OPEN(IUN,FILE=longtfile,ACCESS='SEQUENTIAL',
     &            STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSEIF(IEXIST.EQ.3)THEN

C Use existing file or create a new one.
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile,ACCESS='SEQUENTIAL',
     &            STATUS='OLD',IOSTAT=ISTAT)
        ELSE
          OPEN(IUN,FILE=longtfile,ACCESS='SEQUENTIAL',
     &            STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSEIF(IEXIST.EQ.4)THEN

C IEXIST=4 if file exists, ask user if it should be deleted and
C overwritten with a file by the same name.
        IF(XST)THEN
          OPEN(IUN,FILE=longtfile,ACCESS='SEQUENTIAL',
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
            OPEN(IUN,FILE=longtfile,ACCESS='SEQUENTIAL',
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
          OPEN(IUN,FILE=longtfile,ACCESS='SEQUENTIAL',
     &            STATUS='NEW',IOSTAT=ISTAT)
        ENDIF
      ELSE
        IER=1
        RETURN
      ENDIF

C If problem in opening then report difficulty. F90 ISTAT
C values can be positive numbers, but F77 ISTAT tend to be
C negative values.
      IF(ISTAT.ne.0)THEN
        write(outs,'(a,i5,a)') 'Error (',ISTAT,') opening the file'
        CALL LUSRMSG(outs,longtfile,'W')
        IER=ISTAT
      ENDIF

      RETURN
      END

c ******************** ERPFREE
C ERPFREE is used to close any file.
C IUN (integer) is the file unit number
C ISTAT (integer) is its status 0=OK, 1=if IUN was zero.
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
C IUN (integer) is the file unit number
C ISTAT (integer) is its status 0=OK, 1=if IUN was zero.
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

C ************ c2fstr
C c2fstr: converts c function returned string to fortran format.
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

C Debug.
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

C ******************** addpath
C ADDPATH: Return file name appended onto the path and logical concat.
C IUOUT is the message channel, SFILE is the file name.
C If the path does not begin with '/' or '?:' then
C concatenate path with sfile.
C If path is '  ' or './' do not concatenate
C This version includes cross-platform logic.
      SUBROUTINE addpath(SFILE,tfile,concat)
      IMPLICIT NONE
      
C Declare calling parameters
      CHARACTER*(*), INTENT(OUT) :: sfile,tfile
      LOGICAL, INTENT(OUT) :: concat
C Declare local variables
      LOGICAL :: unixok
      CHARACTER :: fs,bs,a1
      CHARACTER(72) :: path
      CHARACTER(12) :: tp, tp1
      CHARACTER(124):: outs
      INTEGER :: LN,LNS,I,LT,ios,iuin,iuout,ieout,pos
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/rpath/path
      
C Define path and file name lengths and check operating system.
      LN=max(1,lnblnk(path))
      LNS=max(1,lnblnk(sfile))
      call isunix(unixok)

C Set folder separator (fs) to \ or / and the folders to ignore as required.
      fs = char(47)
      bs = char(92)
      write(tp,'(2a)') '.',fs
      write(tp1,'(2a)') '.',bs
      write(a1,'(a)') sfile(1:1)
C Debug.
C      write(*,*)'tp',tp,'tp1',tp1,'a1',a1

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
            if (tfile(I:I).eq.bs) then
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
C make sure all \ (bs) are replaced with / (fs)
         do
           pos=SCAN(sfile,bs)
           if (pos.ne.0) then
             sfile(pos:pos)=fs
           else 
             EXIT
           end if
         end do      

C Special case: unix absolute path being used on windows - substitute c: for initial /
C Limit feedback text length to fit within outs text buffer.
          concat = .false.
          write (tfile,'(2a)') 'C:',sfile(5:LNS)
          sfile=tfile
          write(outs,*)
     &      'addpath: warning found unix path changing to windows.'
          call edisp(iuout,outs)
          LNS=lnblnk(sfile)
          if(LNS.gt.112) LNS=112
          write(outs,*) 'addpath: ',sfile(1:LNS)
          call edisp(iuout,outs)
        endif
        
      endif

      if(concat)then
        LN=max(1,lnblnk(path))
        LNS=max(1,lnblnk(sfile))
        write(tfile,'(2a)',iostat=ios,err=1)path(1:LN),sfile(1:LNS)
      else
        write(tfile,'(a)',iostat=ios,err=1)sfile(1:LNS)
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

C ****** Date stamp routine.
C tstamp: generates a timestamped message.
C act (char*1) = '-' Current time, = 'm' Message plus time
C  = '>' append message to journal
      subroutine tstamp(act,msg)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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
            open(iuj,file=jfile(1:ltf),position='APPEND',
     &        status='UNKNOWN',err=1)
            LN=max(1,lnblnk(outs))
            write(iuj,'(a)',iostat=ios,err=2)outs(1:LN)
            close(iuj)
          endif
        endif
      endif

      return

   1  if(IOS.eq.2)then
        call edisp(iuout,
     &  'Permission issue while opening journal file, continuing.')
      else
        call edisp(iuout,'Error opening journal file, continuing.')
      endif
      return
   2  if(IOS.eq.2)then
        call edisp(iuout,
     &  'Permission issue while updating journal file, continuing.')
      else
        call edisp(iuout,'Error updating journal file, continuing.')
      endif
      close(iuj)
      return
      end

C ****** ectime function
C ectime function as fixed length character*24 based on the
C the passed value of ictime
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

C ****** dstamp
C dstamp: Get date stamp in the form: Fri Jan 23 09:34:31 1998.
C Used to isolate code from system details.
      subroutine dstamp(date_str)
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer time
      integer lnblnk
      integer lns
      character date_str*24
      character ectime*24

C Unix date function. If date_str blank assign a timestamp.
      ictime=time()
      write(date_str,'(A)',iostat=ios,err=1) ectime(ictime)
      lns=lnblnk(date_str)
      if(lns.gt.20)then
        continue
      else
        date_str='Mon Feb 11 16:42:00 2013'
      endif
      return

C If an error detected assign a timestamp.
   1  if(IOS.eq.2)then
        call edisp(iuout,'dstamp: permission error getting time.')
        date_str='Mon Feb 11 16:42:00 2013'
      else
        call edisp(iuout,'dstamp: error getting time, using fixed date')
        date_str='Mon Feb 11 16:42:00 2013'
      endif
      return
      end

C ******************** STRIPC1K
C STRIPC1K strips comments from a ASCII file (1000 char long) string and returns the data.
C It assumes that if a string begins with a '#' then the whole line is
C a comment an the next line is read.  If a ' #' is discovered within
C a line the rest of the line is removed.
C IER=0 if ok. MSG is a text string used in error messages. If
C IR=0 then acts silently, otherwise notes when EOF found.
C IEXP is the number of expected items in the line:
C   IEXP = 0 means don't care or already know no. items - don't check
C   IEXP >0  means a specific number of items expected (error if not)
C   IEXP = 99 check number of items and return in ITEMS

      SUBROUTINE STRIPC1K(INPCH,OUTSTR,IEXP,ITEMS,IR,MSG,IER)
#include "espriou.h"

      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      CHARACTER*1000 OUTSTR,STRING,TMP
      CHARACTER loutstr*248,outs*124
      character truncstr*248   ! truncate outsr for reporting.
      CHARACTER*(*) MSG
      logical unixok

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

C For messages we need to remember the initial part of the long string.
      lastcc = MIN0(LNBLNK(OUTSTR),247)
      write(truncstr,'(a)') OUTSTR(1:lastcc)

C Find out the number of separate words/string groupings.
      if(IEXP.eq.99)then
        ITEMS = iCountWords(OUTSTR)
      elseif(IEXP.eq.0)then
        ITEMS=0
      elseif(IEXP.gt.0)then
        ITEMS = iCountWords(OUTSTR)
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
        lastcc = MIN0(LNBLNK(STRING),247)
        write(truncstr,'(a)') STRING(1:lastcc)
        CALL LUSRMSG(loutstr,truncstr,'W')
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

   1  call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      if(IOS.eq.2)then
        write(outs,*) 'LSTRIPC4: permission error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,outstr(1:72))
      else
        write(outs,*) 'LSTRIPC4: error writing warning re: '
        call edisp(iuout,outs)
        call edisp(iuout,outstr(1:72))
      endif
      return

      END

C ********************* iCountWords
C iWordsInString checks a character string (A), returning the number of data
C items (IW) separated by ' ' tab or ','.  Note to keep from
C overwriting the string passed, deal with a copy.
      integer function iCountWords(cString)
      implicit none
      character*(*) cString
      character*1   cChar
      integer iLastChar, iCurChar
      logical bLastCharWasSpace
      integer lnblnk
      integer iChar

C.....Get length of string      
      iLastChar = lnblnk(cString)

C.....Now search for words

      bLastCharWasSpace = .true.
      iCountWords       = 0

      SearchForWords: do iCurChar=1, iLastChar

        cChar = cString(iCurChar:iCurChar)

        IsNonBlank: if ( cChar /= ' '      .and.
     &                   cChar /= ','      .and.
     &                   iChar(cChar) /= 9        ) then

C.........If character is non-blank and last character
C.........was blank, comma or tab, increment word coumt
          IsNewWord: if ( bLastCharWasSpace ) then

            iCountWords = iCountWords + 1

          endif IsNewWord

          bLastCharWasSpace = .false.

        else

          bLastCharWasSpace = .true. 

        endif IsNonBlank

      enddo SearchForWords

      return
      end function iCountWords



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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
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

C ********************* clrtextbuf
C clrtextbuf clears the text buffer common blocks.
      subroutine clrtextbuf()
      common/textbuf/dispbuf(500)
      common/textbufl/indexbuf,lnbuf(500)
      character dispbuf*144

      indexbuf=0
      do 42 i=1,500
        dispbuf(i)='  '
        lnbuf(i)=1
  42  continue
      return
      end



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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/textbuf/dispbuf(500)
      common/textbufl/indexbuf,lnbuf(500)
      CHARACTER*(*) MSG
      CHARACTER WWMSG*144,dispbuf*144
      logical unixok

C Function determining if HOT3000 features enabled?
C      logical bH3KExtentionsActive
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
c.......write console output (ITRU=6) to file       
C        if(ITRU.eq.6 .and. bH3KExtentionsActive() )then
C            call redir_console_output_to_file(MSG(1:lnm)//CHAR(0))
C        endif
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

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE EMPAGE(IPAG,IW,IEND)
C EMPAGE: Low level screen control for paging based on terminal MMOD.
C The available terminal see EPAGES.
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
     &  menuchw,TITLE,lntitle)

C Parameters
      integer lntitle
      character (len=lntitle) :: TITLE
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer ifs,itfs,imfs
      common/gfont/ifs,itfs,imfs
      CHARACTER HEAD*255

      CHARACTER*5 MM
      integer ifont   ! to pass to winfnt
      integer immod   ! to pass to jwinint

c If MMOD set to valid index, assume it, dont ask.
      MM='     '
  10  IF(MM(1:5).EQ.'-6   '.OR.MODEL.EQ.-6)THEN
        MMOD=-6
        LIMTTY=24
        LIMIT=24
        RETURN
      ELSEIF(MM(1:5).EQ.'-2   '.OR.MODEL.EQ.-2)THEN
        MMOD=-2
        LIMTTY=24
        LIMIT=24
        RETURN
      ELSEIF(MM(1:5).EQ.'-1   '.OR.MODEL.EQ.-1)THEN
        MMOD=-1
        LIMTTY=24
        LIMIT=24
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
        IF(MM(1:5).EQ.'-6   '.OR.MM(1:5).EQ.'-2   '.OR.
     &     MM(1:5).EQ.'-1   '.OR.MM(1:5).EQ.'8    ')THEN
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

C ******************** ASKOK
C ASKOK Generic choice facility returning logical variable
C from a yes no (with default indicated) prompt (supports help).
C In the case of a graphic menu the messages will appear in a
C dialogue box at the bottom of the graphic window.
C If NHELP=-1 there is no default option.
      SUBROUTINE ASKOK(MSG1,MSG2,OK,DOK,NHELP)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      CHARACTER*(*) MSG1,MSG2
      CHARACTER ANS*2,MSG3*124,outs*124
      logical ok,dok,DEFLT
      integer IWI  ! for radio button

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
     &      ' ',' ',IWI,newnbhelp)
          else
            call EASKATOG(MSG1,MSG2,'yes','no (default)',' ',' ',' ',
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
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

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

C ************************ dupphelp
C dupphelp copies current pop-uphelp common into a string array so that
C C code does not destroy it.
      SUBROUTINE dupphelp(dh)
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      dimension dh(60)
      CHARACTER dh*72,outs*124

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
#include "help.h"
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

C ************************ PHELPD
C PHELPD displays the current contents of common pophelp in a form
C appropriate to the current terminal type. MSG is a short descriptive
C string for the subject. NHELP is the number of lines to be displayed.
C IBX & IBY are the preferred coords of the lower left corner if in
C terminal type 8.
      SUBROUTINE PHELPD(MSG,NHELP,OTHER,IBX,IBY,IER)
#include "help.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C dh is duplicate help list.
      dimension dh(60),lh(36)
      CHARACTER*(*) MSG,OTHER
      CHARACTER outs*73,dh*72,lh*72
      character IA*3
      integer IBX1,IBY1

C Set coordinates IBX and IBY to zero (so parameter not needed).
      IBX1=0; IBY1=0
      IPM = 1

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

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      CHARACTER ANS*2,MSG3*248

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
      ENDIF
      RETURN

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



C subroutine to generate line of help text for an string dialog
C if no lines of help have been defined in the calling code. 
      subroutine helpwithblank(ermsg,nbhelp,newnbhelp,ier)
#include "help.h"
      
      integer lnblnk  ! function definition

C Parameters.
      CHARACTER*(*) ERMSG  ! error message from dialog
C      integer nbhelp       ! original nb of help lines
      integer newnbhelp    ! number of help lines after additional line(s)
      integer ier          ! zero is ok

C Commons.
      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Local variables.
      integer ln           ! length of error message
      character outs*124 ! for messages

C Get to work.
      IF(nbhelp.GT.0)THEN
        newnbhelp=nbhelp  ! nothing to do
      ELSE
        LN=max(1,LNBLNK(ERMSG))
        newnbhelp=1
        WRITE(H(1),64,iostat=ios,err=1)ERMSG(1:LN)
   64   FORMAT('No help available for ',a,'!')
      ENDIF

      return

   1  if(IOS.eq.2)then
        write(outs,*) 
     &    'helpwithblank: permission error composing help.'
        call edisp(iuout,outs)
        ier=2
      else
        write(outs,*) 'helpwithblank: error composing help.'
        call edisp(iuout,outs)
        ier=1
      endif
      return
      end


