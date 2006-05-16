C Supplimentatl code for HP compiler. Should be included
C in the Makefile of lib/esru.

C **** lnblnk
C Finds the last non-blank poisiton in "string".
      function lnblnk(string)
      character*(*) string
      character a*1,sl*1
      left=len(string)
      lnblnk=left
 42   continue
      left=left-1
      if(left.le.0)then
        lnblnk = 1
        return
      else
        a=string(left:left)
        if(ichar(a).gt.32)then
          lnblnk = left+1
          return
        elseif(ichar(a).eq.32.or.ichar(a).eq.9)then
          goto 42
        endif
      endif
      end

C This is a place-holder for HP compiler << IT SHOULD BE
C REPLACED when the relevent system call has been found.
C      subroutine fdate(TUNIX)
C      character*24 TUNIX
C      TUNIX='Thu Aug 12 18:49:43 1999'
C      return
C      end

