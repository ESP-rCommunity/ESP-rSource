C Written by Jon Hand, December 2010 updated Jan/Feb 2011

C Console application to take a text file of the
C source of an esp-r module (usually created by a
C cat *.F >all_source command and then moving this
C to source.F (which this application expects to find
C in the same folder as this executable).
C It opens the source.F file and if the source line
C is of type h(1)='xx' or H(1)='xx' etc. it writes out
C these lines into a separate text file (source.FH) 
C which is based on the original name of the source file.

C To compile (use either f90 or gfortran):
C f90 -c scan_context.F -g
C f90 -o scan_context scan_context.o
C gfortran -c scan_context.F -g
C gfortran -o scan_context scan_context.o
C gfortran-4.3 -c scan_context.F -g
C gfortran-4.3 -o scan_context scan_context.o

C Further editing of source.FH in order for it
C to function as a module_name.help file for scanning
C by ESP-r modules.  Compare source.FH with esruclm.help
C to get an idea of what is needed and see the source
C of subroutine gethelptext for specifics.

      program scan_context

      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN
      character fileinput*72
      character fileoutput*72
      character outs*124
      CHARACTER*42 WORDS(24)
      integer lnl  ! length of outs
      integer lneq ! position of equals
      integer lnparen  ! position of (
      integer ij   ! loop counter
      CHARACTER B*124,C*1
      character head*72    ! the *item line
      character buffer*124 ! to remember the help lines
      dimension buffer(200)
      integer buflen(200)  ! length of each line in the buffer
      integer buflines     ! number of buffer lines recorded
      character cursub*42  ! name of most recent subroutine
      integer lncursub     ! length of most recent subroutine
      integer nexttok      ! position of next token in CALL

C Initial values and clear arrays.
      write(6,'(a)') 'Assumes input file is source.F and that the'
      write(6,'(a)') 'output file is source.FH'
      IUOUT=6
      IUIN=5
      cursub='MAIN'   ! assume we are in MAIN until we get a subroutine
      lncursub=4
      buflines=0      ! assume nothing in the buffer and clear it
      do 21 ij=1,200
        buffer(ij)= ' '
        buflen(ij)= 0
   21 continue
      head = ' '      ! clear heading

C Setup input and output file names.
      write(fileinput,'(a)') 'source.F'    ! assumed name of input source file.
      write(fileoutput,'(a)') 'source.FH' ! assumed file with help text.
      open(1,file=fileinput,status='old')
      open(2,file=fileoutput,status='unknown')

C Opening line of the file. Must be edited by user to insert
C the name of the ESP-r module.
      write(2,'(a)') '*help_text'    ! header of the file

C Loop through all the lines in the input file.
      scanlines: do
        read(1,'(a)',IOSTAT=IOS,ERR=1) outs
        if(ios.eq.-1) exit scanlines
        lnl=lnblnk(outs)
        if(lnl.le.1) cycle  ! jump if a blank line

        call GETTOKENS(outs,IW,WORDS)
        if(IW.le.1) cycle  ! if only one token jump

C Find lines which included h(?)=' ' even if there is a
C label prior to the h(?)=.
C Debug.
C       write(6,*) 'first token ',IW,buflines,' ',words(1)
C        if(IW.gt.1) write(6,*) '2nd token ',words(2)
        if(WORDS(1)(1:2).eq.'h('.or.
     &     WORDS(1)(1:2).eq.'H('.or.
     &     WORDS(2)(1:2).eq.'h('.or.
     &     WORDS(2)(1:2).eq.'H(')then

C Find position of the = and print from that point.
          lastcc = MIN0(LNBLNK(outs),123)
          WRITE(B,'(A)',IOSTAT=ios,ERR=11) outs(1:lastcc+1)

C Loop through B and see what position is =
          lneq=0
          scane: do I=1,LNBLNK(B)+1
            C=B(I:I)
            if(C.EQ.'=')then
              lneq=I
              exit scane
            endif
            cycle
          end do scane

C Write the single quoted string, remember it in buffer as well
C as its length.
          if(lneq.eq.0.or.lneq.eq.1)then
            buflines=buflines+1
            buflen(buflines)=lnblnk(outs)
            write(buffer(buflines),'(a)') outs(1:buflen(buflines))
          else
            buflines=buflines+1
            write(buffer(buflines),'(a)') outs(lneq+1:lnblnk(outs))
            buflen(buflines)=lnblnk(buffer(buflines))  ! find actual
          endif

C If the 1st or 2nd token is CALL the proceed.
        elseif(WORDS(1)(1:4).eq.'CALL'.or.
     &         WORDS(1)(1:4).eq.'call'.or.
     &         WORDS(2)(1:4).eq.'CALL'.or.
     &         WORDS(2)(1:4).eq.'call')then
          if(WORDS(1)(1:4).eq.'CALL'.or.
     &       WORDS(1)(1:4).eq.'call')then
             nexttok=2
          elseif(WORDS(2)(1:4).eq.'CALL'.or.
     &       WORDS(2)(1:4).eq.'call')then
             nexttok=3
          endif        
          if(WORDS(nexttok)(1:6).eq.'PHELPD'.or.
     &       WORDS(nexttok)(1:6).eq.'phelpd')then

C We reached the next subroutine so dump out the head and buffer.
            if(lncursub.le.1)then
              write(head,'(a)') '*item'
            else
              write(head,'(2a)') '*item ',cursub(1:lncursub)
            endif
            write(2,'(a)') head(1:lnblnk(head))
            if(buflines.gt.0)then
              do 22 ij=1,buflines
                write(2,'(a)') buffer(ij)(1:buflen(ij))
   22         continue
            endif
            write(2,'(a)') '*enditem'  ! mark end of help text block
            write(2,'(a)') outs(1:lnblnk(outs))
            head=' '     ! reset
            buflines=0

          elseif(WORDS(nexttok)(1:4).eq.'EASK'.or.
     &           WORDS(nexttok)(1:4).eq.'eask')then
            if(lncursub.le.1)then
              write(head,'(a)') '*item'
            else
              write(head,'(2a)') '*item ',cursub(1:lncursub)
            endif
            write(2,'(a)') head(1:lnblnk(head))
            if(buflines.gt.0)then
              do 23 ij=1,buflines
                write(2,'(a)') buffer(ij)(1:buflen(ij))
   23         continue
            endif
            write(2,'(a)') '*enditem'  ! mark end of help text block
            write(2,'(a)') outs(1:lnblnk(outs))
            head=' '     ! reset
            buflines=0
          elseif(WORDS(nexttok)(1:3).eq.'ASK'.or.
     &           WORDS(nexttok)(1:3).eq.'ask')then
            if(lncursub.le.1)then
              write(head,'(a)') '*item'
            else
              write(head,'(2a)') '*item ',cursub(1:lncursub)
            endif
            write(2,'(a)') head(1:lnblnk(head))
            if(buflines.gt.0)then
              do 24 ij=1,buflines
                write(2,'(a)') buffer(ij)(1:buflen(ij))
   24         continue
            endif
            write(2,'(a)') '*enditem'  ! mark end of help text block
            write(2,'(a)') outs(1:lnblnk(outs))
            head=' '     ! reset
            buflines=0
          elseif(WORDS(nexttok)(1:8).eq.'MENUATOL'.or.
     &           WORDS(nexttok)(1:8).eq.'menuatol')then
            if(lncursub.le.1)then
              write(head,'(a)') '*item'
            else
              write(head,'(2a)') '*item ',cursub(1:lncursub)
            endif
            write(2,'(a)') head(1:lnblnk(head))
            if(buflines.gt.0)then
              do 25 ij=1,buflines
                write(2,'(a)') buffer(ij)(1:buflen(ij))
   25         continue
            endif
            write(2,'(a)') '*enditem'  ! mark end of help text block
            write(2,'(a)') outs(1:lnblnk(outs))
            head=' '     ! reset
            buflines=0
          elseif(WORDS(nexttok)(1:6).eq.'EPICKS'.or.
     &           WORDS(nexttok)(1:6).eq.'epicks')then
            if(lncursub.le.1)then
              write(head,'(a)') '*item'
            else
              write(head,'(2a)') '*item ',cursub(1:lncursub)
            endif
            write(2,'(a)') head(1:lnblnk(head))
            if(buflines.gt.0)then
              do 26 ij=1,buflines
                write(2,'(a)') buffer(ij)(1:buflen(ij))
   26         continue
            endif
            write(2,'(a)') '*enditem'  ! mark end of help text block
            write(2,'(a)') outs(1:lnblnk(outs))
            head=' '     ! reset
            buflines=0
          elseif(WORDS(nexttok)(1:6).eq.'FLNAME'.or.
     &           WORDS(nexttok)(1:6).eq.'flname')then
            if(lncursub.le.1)then
              write(head,'(a)') '*item'
            else
              write(head,'(2a)') '*item ',cursub(1:lncursub)
            endif
            write(2,'(a)') head(1:lnblnk(head))
            if(buflines.gt.0)then
              do 27 ij=1,buflines
                write(2,'(a)') buffer(ij)(1:buflen(ij))
   27         continue
            endif
            write(2,'(a)') '*enditem'  ! mark end of help text block
            write(2,'(a)') outs(1:lnblnk(outs))
            head=' '     ! reset
            buflines=0
          endif
          cycle    ! loop back for more
        elseif(WORDS(1)(1:10).eq.'SUBROUTINE'.or.
     &         WORDS(1)(1:10).eq.'subroutine')then

C Find position of the ( in 2nd token and print up to that point.
          lastcc = LNBLNK(WORDS(2))
          WRITE(B,'(A)',IOSTAT=ios,ERR=11) WORDS(2)(1:lastcc+1)

C Loop through B and see what position is (
          lnparen=0
          scanp: do I=1,LNBLNK(B)+1
            C=B(I:I)
            if(C.EQ.'(')then
              lnparen=I
              exit scanp
            endif
            cycle
          end do scanp

          if(lnparen.eq.0.or.lnparen.eq.1)then

C If ( not found then just write out subroutine name.
            write(2,'(2a)') '*subroutine ',WORDS(2)(1:lnblnk(WORDS(2)))
            lncursub=lnblnk(WORDS(2))
            write(cursub,'(a)') WORDS(2)(1:lncursub)
            lncursub=lnblnk(cursub)
          else

C If ( found then write out characters prior to (
            lncursub = lnparen-1
            write(2,'(2a)') '*subroutine ',WORDS(2)(1:lncursub)
            write(cursub,'(a)') WORDS(2)(1:lncursub)
          endif
        elseif(WORDS(1)(1:7).eq.'PROGRAM'.or.
     &         WORDS(1)(1:7).eq.'program')then

C If we find a line like program clm remember the clm part.
          lastcc = LNBLNK(WORDS(2))
          write(cursub,'(a)') WORDS(2)(1:lastcc)
          lncursub=lnblnk(cursub)
        else
          cycle    ! loop back for more
        endif
        cycle    ! loop back for more
      end do scanlines

C The jump out point.
  1   close(1)
      close(2)
      write(6,'(a)') 'Reached the end of the file'
      stop

  11  if(IOS.eq.2)then
        write(outs,*) 'GETTOKENS: internal write permission error: ',B
        call edisp(iuout,outs)
      else
        write(outs,*) 'GETTOKENS: error in internal write: ',B
        call edisp(iuout,outs)
      endif
      stop   ! after writing the error message

      end

C ********************* GETTOKENS
C GETTOKENS checks a character string (A), returning the number of data
C items (IW) separated by a file separator and an array of tokens/words
C (up to 12 32character words returned) that were in the string.
C Useful for parsing file paths.  Note to keep from
C overwriting the string passed, deal with a copy.
      SUBROUTINE GETTOKENS(A,IW,WORDS)
      integer iuout,iuin
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER*(*) A
      CHARACTER*42 WORDS(24)
      CHARACTER B*124,C*1,outs*248
      integer ier

C Determine the file separator and clear the WORDS to return.
      do 9 i=1,24
        WORDS(i)='  '
  9   continue
      ier=0

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
            CALL EGETW(B,KK,WORDS(ij),'-','token',IER)
            if(ier.ne.0)goto 2
  42      continue
        endif
        RETURN
      endif
      IF(C.EQ.' ')GOTO 102
      GOTO 20
   1  if(IOS.eq.2)then
        write(outs,*) 'GETTOKENS: internal wreite permission error: ',A
        call edisp(iuout,outs)
      else
        write(outs,*) 'GETTOKENS: error in internal write: ',A
        call edisp(iuout,outs)
      endif
      return
   2  if(IOS.eq.2)then
        write(outs,*) 'GETTOKENS: permission error in getting token: ',
     &    ij,' ',A
        call edisp(iuout,outs)
      else
        write(outs,*) 'GETTOKENS: error in getting token: ',ij,' ',A
        call edisp(iuout,outs)
      endif
      return

      END

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
      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN

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
        LNM=max(1,lnblnk(MSG))
        WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &    'WARNING: Past end of line for ',
     &    MSG(1:LNM),' (word) in...'
      elseif(ACT.EQ.'F')then
        ier=1
        LNM=max(1,lnblnk(MSG))
        WRITE(loutstr,'(3a)',IOSTAT=IOS,ERR=1)
     &    'FAILURE: Past end of line for ',
     &    MSG(1:LNM),' (word) in...'
      endif
      CALL EDISP(iuout,loutstr)
      CALL EDISP(iuout,STRING)
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
      CHARACTER*(*) MSG
      logical unixok

C Create matching string to pass to wwlib.c and hold in dispbuf.
C Use logic similar to that in egdisp.
      lnm=max0(1,lnblnk(MSG))

C Write this to standard text window.
      WRITE(ITRU,'(A)',iostat=ios,err=1)MSG(1:lnm)

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
      END

