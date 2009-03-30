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

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C PCENTR of ESRUpdb
C Enter or import a component into the datatbase.
C Data can be entered from a file or the user can be
C prompted for each field of the plant component entry.
C The routine 'PCEDIT' can be used to edit
C user input data before the routine.
C 'PCPUT' is called to add it to the datatbase.
        SUBROUTINE PCENTR
#include "plantdb.h" 
      
      integer lnblnk  ! function definition

      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      COMMON/ERRS/ISTAT,IREC
      EQUIVALENCE (ERRFLG, ISTAT)

      common/PCDATB/ICODE,INDXPC,IRC,NNODE,NMATX,NMISC,
     &              nadata,nbdata,ncdata,napout,
     &              NDCON(MAXNOD),ISV(MAXNOD),NCPOS(MAXMTX),
     &              dtdesc(maxmsc),DATAMS(MAXMSC),range(maxmsc,2),
     &              adodsc(mxaout),noutyp(mxaout)

      character dtdesc*68,adodsc*30,ZDESC*25 
      character LTMP*72,ltmpg*72,buffer*90,H*72
      LOGICAL OK,dok

C Build menu items.
 100  h(1)='The input file should be identical in format to that  '
      h(2)='exported from pdb.'
      CALL EASKABCD('Read component data:',' ','from file',
     &  'from user dialogue','from file to generic','continue',irt,2)
      if(irt.eq.1.or.irt.eq.3)then

C Read from file, get filename
        CALL EPAGE
        h(1)='The ACII file holds plant component entries.'
        h(2)='If found, components data is read in and inserted'
        h(3)='in plant component database.'
        h(4)=' '
        h(5)='Note that the format of components data description'
        h(6)='is identical to that obtained from ESRUpdb (list).'
        ltmp=' '
  205   CALL EASKS(LTMP,' ASCII file name? ',' ',72,' ',
     &    'ASCII file',IER,6)

        IUFIL = IFIL+1
        CALL FPOPEN (IUFIL,ISTAT,1,1,LTMP)
        IF (ISTAT .EQ. -3)   GOTO 100
        IF (ISTAT .NE. 0)goto 205

C Read components data from file
        READ (IUFIL,2) ZDESC
    2   FORMAT (A25,/)
        IF(ZDESC(1:24).EQ.' PLANT COMPONENT LIBRARY') THEN
          call usrmsg(' ',' File is obsolete.','-')
          goto 100
        ELSE IF(ZDESC(1:25).NE.' PLANT COMPONENT DATABASE') THEN
          call usrmsg(' ',' Not a PLANT COMPONENT DATABASE!','-')
          goto 100
        END IF

C Fill the generic header.
        if(irt.eq.3)then
          write(ltmpg,'(2a)') ltmp(1:lnblnk(ltmp)),'.gdba'
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
     &     ' Plant component entry ',INDXPC,'added'
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
        CALL EPAGE
        CALL PCRD
        IF (INDXPC .LE. 0)   GOTO 100

C Check if user accepts data.
        CALL EPAGE
        CALL PCWRT (IUOUT)
        dok=.true.
        h(1)='Pause to review data. No returns to editing.'
        CALL ASKOK(' ',' Is this OK? ',OK,dok,1)
        IF(OK)GOTO 330

C Changes required, so call 'PCEDIT'
        CALL PCEDIT(icode)
        GOTO 100

C Put data into database
  330   CALL PCPUT (INDXPC)
        GOTO 100
      elseif(irt.eq.4)then
        return
      endif
      END
