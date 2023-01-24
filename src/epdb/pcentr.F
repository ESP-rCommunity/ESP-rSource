C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C ******************** PCENTR ********************
C Enter or import a component into the datatbase.
C Data can be entered from a file or the user can be
C prompted for each field of the plant component entry.
C The routine 'PCEDIT' can be used to edit
C user input data before the routine.
C 'PCPUT' is called to add it to the datatbase.

        SUBROUTINE PCENTR
#include "plantdb.h" 
#include "pdb_data.h" 
#include "help.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      character lcmdfl*144
      common/rcmd/LCMDFL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      character PZDESC*25 
      character ltmpg*144,buffer*90
      character lltmp*144,L144*144
      LOGICAL OK

      helpinsub='pcentr' ! set for subroutine

C Build menu items.
 100  helptopic='pdb_input_file_choices'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABCD(' ','Read component data:','from file',
     &  'from user dialogue','from file to generic','cancel',irt,
     &  nbhelp)

      if(irt.eq.1.or.irt.eq.3)then

C Read from file, get filename
  205   lltmp=' '  ! clear file name to be returned
        call edisp(iuout,'  ')  ! echo blank line

C Set initial guess for import file to command line file +a
        write(L144,'(2a)') LCMDFL(1:lnblnk(LCMDFL)),'a'  
        CALL EASKXORGTKF(L144,
     &    'ASCII import file name','confirm','importfile.dta',
     &    lltmp,'import plant templates',IER,nbhelp)

C If user cancel display the menu without loading file.
        if(ier.eq.-3)then
          return  ! re-display menu.
        endif

        IUFIL = IFIL+1
        CALL FPOPEN (IUFIL,ISTAT,1,1,LLTMP)
        IF (ISTAT .EQ. -3)   GOTO 100
        IF (ISTAT .NE. 0)goto 205

C Read components data from file
        READ (IUFIL,2) PZDESC
    2   FORMAT (A25,/)
        IF(PZDESC(1:24).EQ.' PLANT COMPONENT LIBRARY') THEN
          call usrmsg(' ',' File is obsolete.','-')
          goto 100
        ELSE IF(PZDESC(1:25).NE.' PLANT COMPONENT DATABASE') THEN
          call usrmsg(' ',' Not a PLANT COMPONENT DATABASE!','-')
          goto 100
        END IF

C Fill the generic header.
        if(irt.eq.3)then
          write(ltmpg,'(2a)') lltmp(1:lnblnk(lltmp)),'.gdba'
          IUFILG = IFIL+2
          CALL FPOPEN (IUFILG,ISTAT,1,3,LTMPG)
          call PCRDFGDB(IUFIL,'h')
        endif

  210   if(irt.eq.1)then
          CALL PCRDF(IUFIL)
        elseif(irt.eq.3)then
          call PCRDFGDB(IUFIL,'g')
        endif
        IF(INDXPC.GT.0) THEN
          CALL PCPUT (INDXPC)
          WRITE(buffer,'(a,i5,a)')
     &     ' Plant component entry ',INDXPC,' added'
          call edisp(iuout,buffer)
          GOTO 210
        ELSE IF(INDXPC.LT.0) THEN
          GOTO 210
        ELSE
          CALL ERPFREE(IUFIL,ISTAT)
          CALL ERPFREE(IUFILG,ISTAT)
          GOTO 100
        END IF

      elseif(irt.eq.2)then

C Read component data from user.
        CALL PCRD
        IF (INDXPC .LE. 0)   GOTO 100

C Check if user accepts data.
        CALL PCWRT (IUOUT)
        helptopic='pdb_input_review_pause'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Is this OK?',OK,nbhelp)
        IF(OK)GOTO 330

C Changes required, so call 'PCEDIT'.
        CALL PCEDIT(icode)
        GOTO 100

C Put data into plant template file.
  330   CALL PCPUT (INDXPC)
        GOTO 100

      elseif(irt.eq.4)then
        return
      endif
      END
