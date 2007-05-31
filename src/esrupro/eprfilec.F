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

C  TRIPRO:  Reads data supplied from an ASCII profiles db and writes
C           it to a NEW events profiles database file.
C  TROPRO:  Creates an ASCII profiles db from the binary profiles db.

C ***************** TRIPRO 
C TRIPRO reads data supplied from an ASCII profiles db and writes
C it to a NEW events profiles database file.
      SUBROUTINE TRIPRO(ITRU,IFA,LASCI,IER)
      INCLUDE 'esprdbfile.h'
C esprdbfile.h supplies the following:
C LPRFDB,IPRODB (for event profile database)

      common/pophelp/h(60)
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER DESC*40,OUTSTR*124,LASCI*72,h*72
      LOGICAL OK,dok

C The binary file will be overwritten.
      dok=.true.
      h(1)='Reading in an ASCII version of the profiles database '
      h(2)='will overwrite the current profiles database. If this '
      h(3)='is ok say yes. If not say no so you can respecify. '
      CALL ASKOK(' ','Ok to overwrite the profiles database?',OK,dok,3)
      IF(.NOT.OK)THEN
        IER=1
        RETURN
      ELSE
        CALL ERPFREE(IPRODB,ISTAT)
        call EFOPRAN(IPRODB,LPRFDB,10,3,IER)
        if(ier.ne.0)return
      ENDIF

C Open the ASCII file for reading.
      CALL EFOPSEQ(IFA,LASCI,1,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' ',' ASCII file does not exist!','W')
        IER=1
        RETURN
      ENDIF

C Proceed.
      CALL STRIPC(IFA,OUTSTR,0,ND,1,'no items',IER)
      IF(IER.NE.0)RETURN
      K=0
      CALL EGETWI(OUTSTR,K,NITEMS,0,0,'-','no items',IER)

      IREC = 1
      WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NITEMS

C Get number of events and description of profile (EGETRM gets the
C remainder of the line after the first word (NCG).
      DO 10 I = 1,NITEMS
        CALL STRIPC(IFA,OUTSTR,0,ND,1,'NCG,DESC',IER)
        IF(IER.NE.0)RETURN
        K=0
        CALL EGETWI(OUTSTR,K,NCG(I),0,0,'-','NCG',IER)
        CALL EGETRM(OUTSTR,K,DESC(I),'W','profile descr',IER)

        IREC = IREC + 1
        NO(I)=IREC
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        IREC = IREC + 1
        WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  DESC(I)

        DO 20 J = 1,NCG(I)
          CALL STRIPC(IFA,OUTSTR,0,ND,1,'IS,IF,A,B',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,IS,0,0,'-','Start',IER)
          CALL EGETWI(OUTSTR,K,IF,0,0,'-','Finish',IER)
          CALL EGETWR(OUTSTR,K,A,0.,0.,'-','VAL 1',IER)
          CALL EGETWR(OUTSTR,K,B,0.,0.,'-','VAL 2',IER)
          IREC = IREC + 1
          WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
   20   CONTINUE
   10 CONTINUE

      IR=IREC+1
      IREC = 1
      WRITE(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NITEMS,IR

  99  CALL ERPFREE(IPRODB,ISTAT)
      CALL ERPFREE(IFA,ISTAT)
      RETURN

 1001 WRITE(OUTSTR,1004)IREC
 1004 FORMAT(' File write error ! record',I6)
      CALL USRMSG(' ',OUTSTR,'W')
      goto 99

      END

C **************** TROPRO 
C TROPRO creates an ASCII profiles db from the binary profiles db.
      SUBROUTINE TROPRO(ITRU,IFA,LASCI,IER)
      INCLUDE 'esprdbfile.h'

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PDBITM/NITEMS,lastrec,NCG(100),NO(100),DESC(100)
      CHARACTER DESC*40,OUTSTR*124,LASCI*72

      IER =0

      CALL ERPFREE(IPRODB,ISTAT)
      call EFOPRAN(IPRODB,LPRFDB,10,1,IER)
      if(ier.ne.0)return

      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)NITEMS
      IF(NITEMS.LE.0)THEN
        CALL USRMSG(' ',' Events profiles db is empty !','W') 
        RETURN
      ENDIF

C Open or overwrite the ASCII file.
      CALL EFOPSEQ(IFA,LASCI,4,IER)
      IF(IER.NE.0)THEN
        CALL USRMSG(' Could not open ASCII file!',LASCI,'W')
        IER=1
        RETURN
      ENDIF
      
C Write out a file header.
      WRITE(IFA,30)LASCI(1:lnblnk(LASCI))
  30  FORMAT('# events profiles db defined in ',A)

C Proceed
      IREC=1
      READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NITEMS

      WRITE(IFA,21,IOSTAT=ISTAT,ERR=1000)NITEMS
   21 FORMAT(I5,'    # Number of items')

      DO 10 I = 1,NITEMS
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  NCG(I)
        IREC = IREC + 1
        READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)  DESC(I)

        WRITE(IFA,22)NCG(I),DESC(I)
   22   FORMAT(I5,3X,A40,'  # No periods & description',/,
     &                   '# start finish data data')

C For each period.
        DO 20 J = 1,NCG(I)
          IREC = IREC + 1
          READ(IPRODB,REC=IREC,IOSTAT=ISTAT,ERR=1001)IS,IF,A,B
          WRITE(IFA,23,IOSTAT=ISTAT,ERR=1000)IS,IF,A,B
   23     FORMAT(I4,',',I4,',',F8.3,',',F8.3)
   20   CONTINUE
   10 CONTINUE

  99  CALL ERPFREE(IPRODB,ISTAT)
      CALL ERPFREE(IFA,ISTAT)
      RETURN

 1000 CALL USRMSG(' ',' ASCII file write error in ','W')
      goto 99

 1001 WRITE(OUTSTR,1004)IREC
 1004 FORMAT(' File read error ! record',I6)
      CALL USRMSG(' ',OUTSTR,'W')
      goto 99

      END

