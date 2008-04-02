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

C maintn.f provides facilities for manipulating the instances within
C the TDF scratch file ie. insertion, deleting, copying.
C Written by the staff of ESRU and making use of the ESRU standard 
C library files libesru.a

C The following routines are included:
C  ITINSRT: Is used to insert a new instance.
C  ITDEL:   Is used to delete an instance.
C  ITCOPY:  Is used to copy an instance from (IFOC) to (IFOC2).
C  REFSHG:  Refresh instance with generic attributes (placement, help, indices).


C ************* ITINSRT ************************************************
C ITINSRT is used to insert a new temporal item.
C IFOC, the item array index at the point where the action is to take place. 
C IER=0 OK, IER=1 problem.
      SUBROUTINE ITINSRT(ITRC,IFOC,IER)
#include "tdf2.h"
      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)
      COMMON/TDFGH2/GTYPES(20),GMENUS(20)
      COMMON/TDFG1/GAIDE,GHELP(MHEL),GDHDRT(3),GMH
      common/TDFADDC/iadcols,deftabu(9)
      COMMON/HDAT/IHREC(MIT)
      LOGICAL FOUND

      CHARACTER OUTS*124,msgl2*48,TTAG*12
      CHARACTER TAG*12,TTYPE*8,TMENU*16,TAIDE*36
      CHARACTER GMENUS*16,GTYPES*8,LTDF*72,LTDFA*72
      CHARACTER GAIDE*36,GHELP*72,GDHDRT*72,GMH*32
      CHARACTER H*72

C Now choose one of the generic types to create at the insertion point.
C Copy over default information into instance commons.
      IFG=0
  78  call EBRSTAB(IFG,IER)
      IF(IFG.EQ.0)RETURN

      FOUND=.FALSE.
      CALL GENDAT(GTYPES(IFG),IFOC,FOUND,IER)
      IF(.NOT.FOUND)THEN
        msgl2=' No data found, pick another type!'
        CALL USRMSG(' ',msgl2,'W')
        GOTO 78
      ENDIF

C Set this instances attributes to that of the generic type.
      CALL REFSHG(IFOC,IER)

C Check if the current scratch file can hold additional columns
C of information. If more than MTABC return, if more than nwpr
C then will need to dump to a scratch ascii file (with added width)
C and then
      if((nuwpr+NTBITS(IFOC)).gt.MTABC)then

C If more than MTABC return.
        write(outs,'(3a)') 'The addition of a ',GMENUS(IFG),
     &    ' will exceed the maximum number of database columns.'
        call edisp(iuout,outs)
        ier=2
        return
      elseif((nuwpr+NTBITS(IFOC)).gt.nwpr)then

C If more than nwpr then will need to dump from binary scratch file
C to the ascii file (with added iadcols of width) and then read back
C into a new wider binary scratch file.
        iadcols=(nuwpr+NTBITS(IFOC))-nwpr
        write(outs,'(3a,i2,a)') 'The addition of a ',GMENUS(IFG),
     &    ' will need ',iadcols,' more columns (reformat).'
        call edisp(iuout,outs)
      else

C The additional columns will fit within the current scratch file.
        iadcols=NTBITS(IFOC)
        write(outs,'(3a,i2,a)') 'The addition of a ',GMENUS(IFG),
     &    ' will need ',iadcols,' more columns within current file.'
        call edisp(iuout,outs)
      endif

      TTYPE(IFOC)=GTYPES(IFG)
      TMENU(IFOC)=GMENUS(IFG)

C Ask for the name of this new entry.
      TAG(IFOC)='Undefined'
      H(1)='In order to find items in the TDF db a unique name'
      H(2)='must be assigned to each entry. This name is a '
      H(3)='string up to 12 characters long with no blanks.'
      H(4)='The word UNKNOWN is not acceptable.'
  42  TTAG=TAG(IFOC)
      CALL EASKS(TTAG,' ','Name for entry? ',
     &  12,'new_item','item id',IER,4)
      if(TTAG(1:2).ne.'  '.and.TTAG(1:4).ne.'UNKN')then
        TAG(IFOC)=TTAG
      else
        call edisp(iuout,'That name is not acceptable.')
        goto 42
      endif
      TAIDE(IFOC)=GAIDE

C Knowing the generic type it is now possible to instanciate this
C iten with default timestep data (deftabu). An asci file is then
C written out with extra columns which will then be converted into
C a new scratch file for further editing.
      NITDF=NITDF+1

C Need to predict the next value of ihrec. Do this by setting irec
C to the next position after the last item header. If the first
C item then the position will be 5.
      if(ifoc.gt.1)then
        ihrec(ifoc)=ihrec(ifoc-1)+NTBITS(ifoc-1)+NTSTAT(ifoc-1)+1
      else
        ihrec(ifoc)=5
      endif
      write(6,*) 'lasthd is ',lasthd,' ihrec is ',ihrec

C The total number of fields is the static portion plus the
C timestep portion.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1

C Loop through all fields of the generic type and recover defaults
C for new timestep data (deftabu). IA points back to the data arrays.
      idef=0
      DO 6 J=ISFLDS,IEFLDS
        IA=IATDF(IFOC,J)
        JC=NEXTCL
        NEXTCL=NEXTCL+1
        idef=idef+1

        IF(ITDFTR(IFOC,J).EQ.1)deftabu(idef)=TMAR(IFOC,IA)
        IF(ITDFTR(IFOC,J).EQ.2)deftabu(idef)=1.0
        IF(ITDFTR(IFOC,J).EQ.3)deftabu(idef)=FLOAT(ITMAR(IFOC,IA))
        WRITE(OUTS,'(A,I2,A,I2,A,2I2,a,f8.3)')'Fld:',J,' col:',JC,
     &    ' nexcol:',NEXTCL,idef,' default is ',deftabu(idef)
        CALL EDISP(IUOUT,OUTS)

C Update the column pointer.
        ITCOL(IFOC,J)=JC
    6 CONTINUE

C Write out the ascii file with iiadcols additional columns.
      call usrmsg(' ',
     &   'Updating the ascii file with more columns...','-')
      iiadcols=iadcols
      call ewtdfa(itrc,ltdfa,iutdfa,iiadcols,ier)
      call usrmsg(' ',
     &   'Updating the ascii file with more columns...done.','-')

C Reset iadcols after updating nuwpr for the above actions.
      if(nuwpr.eq.0)then
        nuwpr=iadcols
      else
        nuwpr=nuwpr+iadcols
      endif
      iadcols=0
      call usrmsg(' ','Updating the scratch file...','-')
      call TDFAIMPT(ITRC,ltdfa,iutdfa,'i',IER)
      call usrmsg(' ','Updating the scratch file...done.','P')

C << Is updating NEXTRC required? >>
C      NEXTRC=LASTHD+1

      RETURN

      END

C ************* ITDEL ************************************************
C ITDEL is used to delete an instance in the db.
C IFOC, the item array index at the point where the action is to take place. 
      SUBROUTINE ITDEL(ITRC,IFOC,IER)
#include "tdf2.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD1/TDFDES(MIT,MBITS)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDFTEX/TMAT(MIT,MGAT)
      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

      CHARACTER TAG*12,TTYPE*8,TMENU*16,TAIDE*36
      CHARACTER TDFDES*32,TMAT*16,LTDF*72,LTDFA*72

C To delete a tabular item: compact the columns from IFOC+1 to NITDF

C The following probably needs to be done after manipulation of
C data. This becomes NEXTCl.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      IDECRFLD=NTBITS(IFOC)+NTSTAT(IFOC)+1
      ISFLDS=IEFLDS-NTBITS(IFOC)+1
      NEXTCL=ITCOL(IFOC,ISFLDS)

C Figure out how many fewer columns are needed (for nuwpr)
      ilessfields=NTBITS(IFOC)

C Loop through from IFOC+1 to NITDF and, column by column
C reorganize the column data. IA points back to
C the data arrays. The total number of fields is the static 
C portion plus the period/timestep portion.
      DO 15 I=IFOC+1,NITDF
        IEFLDS=NTBITS(I)+NTSTAT(I)
        ISFLDS=IEFLDS-NTBITS(I)+1
        DO 6 J=ISFLDS,IEFLDS
          IA=IATDF(I,J)

C Note shifting TAB columns does not depend on whether the
C actual field is real, integer or text.
C ITREC is the TAB record for the current timestep. 
C Reading record ITR which holds all of
C the tabular columns for this timestep. Overwrite column JC with
C the value from the old column and then write TABU back out.
          JC=NEXTCL
          DO 45 IP=1,NDBSTP
            ITREC=IP+49
            CALL ERTABU(ITRC,ITREC,IER)

C Must take the old tab column and copy into new tab column and then
C zero the old tab column location.
            TABU(JC)=TABU(ITCOL(I,J))
            TABU(ITCOL(I,J))=0.0
            CALL EWTABU(ITRC,ITREC,IER)
            IF(IER.EQ.1)RETURN
   45     CONTINUE
          TABUMN(JC)=TABUMN(ITCOL(I,J))
          TABUMX(JC)=TABUMX(ITCOL(I,J))
          TABUMN(ITCOL(I,J))=0.0
          TABUMX(ITCOL(I,J))=0.0
          ITCOL(I,J)=JC
   6    CONTINUE
  15  CONTINUE
      NEXTCL=NEXTCL+1

C Decrement the counter to the last record used by the header.
      NEXTRC=NEXTRC-IDECRFLD
      
      CALL EDISP(IUOUT,' ')
      IF(IFOC.LT.NITDF.AND.NITDF.GT.1)THEN

C Delete by compacting array data. Update the number of tabular
C items as appropriate.
        DO 14 I=IFOC,NITDF-1
          TTYPE(I)=TTYPE(I+1)
          TAG(I)=TAG(I+1)
          TMENU(I)=TMENU(I+1)
          TAIDE(I)=TAIDE(I+1)
          NTSTAT(I)=NTSTAT(I+1)
          NTBITS(I)=NTBITS(I+1)
          NTMAT(I)=NTMAT(I+1)
          NTMAR(I)=NTMAR(I+1)
          IFIELD=NTSTAT(I+1)+NTBITS(I+1)
          DO 18 J=1,IFIELD
            TDFDES(I,J)=TDFDES(I+1,J)
            ITDFTR(I,J)=ITDFTR(I+1,J)
            IATDF(I,J)=IATDF(I+1,J)
            ITCOL(I,J)=ITCOL(I+1,J)
            IA=IATDF(I+1,J)
            IF(ITDFTR(I+1,J).EQ.1)THEN
              TMAR(I,IA)=TMAR(I+1,IA)
              TMIN(I,IA)=TMIN(I+1,IA)
              TMAX(I,IA)=TMAX(I+1,IA)
            ELSEIF(ITDFTR(I+1,J).EQ.2)THEN
              TMAT(I,IA)=TMAT(I+1,IA)
            ELSEIF(ITDFTR(I+1,J).EQ.3)THEN
              ITMAR(I,IA)=ITMAR(I+1,IA)
              ITMIN(I,IA)=ITMIN(I+1,IA)
              ITMAX(I,IA)=ITMAX(I+1,IA)
            ENDIF
  18      CONTINUE
  14    CONTINUE
      ENDIF

C Update NITDF, decrement nuwpr, rewrite the scratch file, update
C the ascii file.
      NITDF=NITDF-1
      if(nuwpr-ilessfields.ge.0)then
        write(6,*) 'decrementing from ',nuwpr,' to ',nuwpr-ilessfields
        nuwpr=nuwpr-ilessfields
      endif
      call usrmsg(' ','Updating the scratch file...','-')
      CALL EWTDF(IER)
      call usrmsg(' ','Updating the scratch file...done.','-')

C Write out the ascii file.
      iiadcols=0
      call usrmsg(' ','Updating the ascii file...','-')
      call ewtdfa(itrc,ltdfa,iutdfa,iiadcols,ier)
      call usrmsg(' ','Updating the ascii file...done.','-')

      RETURN
      END

C ************* ITCOPY ************************************************
C ITCOPY is used to copy an instance in the db (IFOC) to (IFOC2).
C Note both instances must be of the same type and IFOC2 should
C have already been created.  The information in IFOC2 is overwritten.
C Tabular columns are copied as required.

      SUBROUTINE ITCOPY(ITRC,IFOC,IFOC2,IER)
#include "tdf2.h"
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD1/TDFDES(MIT,MBITS)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDFTEX/TMAT(MIT,MGAT)
      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

      CHARACTER TAG*12,TTYPE*8,TMENU*16,TAIDE*36
      CHARACTER TDFDES*32,TMAT*16,msgl2*48,LTDF*72,LTDFA*72

      IER=0
      IF(IFOC.EQ.IFOC2.OR.(TTYPE(IFOC2).NE.TTYPE(IFOC)))THEN
        msgl2=' or b) the generic types do not match. '
        CALL USRMSG(
     &  ' Cannot perform this copy because a) the source = destination',
     &  msgl2,'W')
        IER=1
        RETURN
      ENDIF

C Copy IFOC to IFOC2 (execpt for TAG because the user will have already
C created and named that instance before the copy was requested).
C      TAG(IFOC2)=TAG(IFOC)
      TMENU(IFOC2)=TMENU(IFOC)
      TAIDE(IFOC2)=TAIDE(IFOC)
      NTSTAT(IFOC2)=NTSTAT(IFOC)
      NTBITS(IFOC2)=NTBITS(IFOC)
      NTMAT(IFOC2)=NTMAT(IFOC)
      NTMAR(IFOC2)=NTMAR(IFOC)
      IFIELD=NTSTAT(IFOC2)+NTBITS(IFOC2)
      DO 19 J=1,IFIELD
        TDFDES(IFOC2,J)=TDFDES(IFOC,J)
        ITDFTR(IFOC2,J)=ITDFTR(IFOC,J)
        IATDF(IFOC2,J)=IATDF(IFOC,J)
        IA=IATDF(IFOC2,J)
        IF(ITDFTR(IFOC2,J).EQ.1)THEN
          TMAR(IFOC2,IA)=TMAR(IFOC,IA)
          TMIN(IFOC2,IA)=TMIN(IFOC,IA)
          TMAX(IFOC2,IA)=TMAX(IFOC,IA)
        ELSEIF(ITDFTR(IFOC2,J).EQ.2)THEN
          TMAT(IFOC2,IA)=TMAT(IFOC,IA)
        ELSEIF(ITDFTR(IFOC2,J).EQ.3)THEN
          ITMAR(IFOC2,IA)=ITMAR(IFOC,IA)
          ITMIN(IFOC2,IA)=ITMIN(IFOC,IA)
          ITMAX(IFOC2,IA)=ITMAX(IFOC,IA)
        ENDIF
  19  CONTINUE

C For tabular instances.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1
      DO 6 J=ISFLDS,IEFLDS
        IA=IATDF(IFOC,J)

C Note copying TAB columns does not depend on whether the
C actual field is real, integer or text.
C ITR is the TAB record for the current timestep. 
C Read record ITR which holds all of the tabular columns
C for this timestep. Then take the IFOC tab column and copy into IFOC2
C tab column. Also update the max/min data for the new column.
        DO 45 IP=1,NDBSTP
          ITREC=IP+49
          CALL ERTABU(ITRC,ITREC,IER)
          TABU(ITCOL(IFOC2,J))=TABU(ITCOL(IFOC,J))
          CALL EWTABU(ITRC,ITREC,IER)
          IF(IER.EQ.1)RETURN
   45   CONTINUE
        TABUMN(ITCOL(IFOC2,J))=TABUMN(ITCOL(IFOC,J))
        TABUMX(ITCOL(IFOC2,J))=TABUMX(ITCOL(IFOC,J))
   6  CONTINUE

C Save the header section and column max/min and file tail record.
      call usrmsg(' ','Updating the scratch file...','-')
      CALL EWTDF(IER)
      call usrmsg(' ','Updating the scratch file...done.','-')

C Write out the ascii file. << Probably iiadcols should be zero.>>
      iiadcols=0
      call usrmsg(' ','Updating the ascii file...','-')
      call ewtdfa(itrc,ltdfa,iutdfa,iiadcols,ier)
      call usrmsg(' ','Updating the ascii file...done.','-')
      RETURN

      END

C ************* REFSHG ******************************************
C REFSHG Refresh instance with generic attributes (placement, 
C help, indices, default values).
      SUBROUTINE REFSHG(IFOC,IER)
#include "tdf2.h"
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD1/TDFDES(MIT,MBITS)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDFTEX/TMAT(MIT,MGAT)
      COMMON/TDFREL/TMAR(MIT,MGAR),TMIN(MIT,MGAR),TMAX(MIT,MGAR)
      COMMON/TDFINT/ITMAR(MIT,MGAI),ITMIN(MIT,MGAI),ITMAX(MIT,MGAI)
      COMMON/TDFG1/GAIDE,GHELP(MHEL),GDHDRT(3),GMH
      COMMON/TDFG5/GMF(MBITS),MGMA(MBITS)

      DIMENSION SALT(6)
      CHARACTER TDFDES*32,TMAT*16,SALT*16,GMF*32,MGMA*32
      CHARACTER GAIDE*36,GHELP*72,GDHDRT*72,GMH*32
      CHARACTER WORD*20

C Strip info from GMH.
C           NHELP,NBITS,NSTATIC,NGMAT,NGMAR,NGMAI,GERR
C        GMH='2,2,8,1,7,2,-999.0'
      K=0
      CALL EGETWI(GMH,K,NHELP,0,MHEL,'F','help',IER)
      CALL EGETWI(GMH,K,NTBITS(IFOC),1,MBITS,'F','bits',IER)
      CALL EGETWI(GMH,K,NTSTAT(IFOC),0,MGAR,'F','nstat',IER)
      CALL EGETWI(GMH,K,NTMAT(IFOC),0,MGAT,'F','tx fld',IER)
      CALL EGETWI(GMH,K,NTMAR(IFOC),0,MGAR,'F','rl fld',IER)
      CALL EGETWI(GMH,K,NTMAI(IFOC),0,MGAI,'F','in fld',IER)
      IFIELD=NTSTAT(IFOC)+NTBITS(IFOC)

C For each of the fields strip info from TR string created in GENDAT.
      DO 6 J=1,IFIELD
        TDFDES(IFOC,J)=MGMA(J)

C Strip value from the string. Skipping past formatting information.
C GMF(?)='REAL,2,1.0,1.0,365.0'
        K=0
        CALL EGETW(GMF(J),K,WORD,'F','real or text tag',IER)
        IF(WORD(1:4).EQ.'REAL')THEN
          ITDFTR(IFOC,J)=1
          CALL EGETWI(GMF(J),K,IA,1,MGAR,'F','array posn',IER)
          IATDF(IFOC,J)=IA
          CALL EGETWR(GMF(J),K,VAL,0.,0.,'-','default',IER)
          TMAR(IFOC,IA)=VAL
          CALL EGETWR(GMF(J),K,VAL,0.,0.,'-','min ',IER)
          TMIN(IFOC,IA)=VAL
          CALL EGETWR(GMF(J),K,VAL,0.,0.,'-','max ',IER)
          TMAX(IFOC,IA)=VAL
          IF(IER.NE.0)RETURN
        ELSEIF(WORD(1:4).EQ.'TEXT')THEN

C Strip from typical string.
C GMF(?)='TEXT,14,4,1
          ITDFTR(IFOC,J)=2
          CALL EGETWI(GMF(J),K,IVAL,1,MALT,'F','no tx alts',IER)

C If there are any text alternates, find array they are in and then
C read that string and strip out the first as the default.
          IF(IVAL.GT.0)THEN
            CALL EGETWI(GMF(J),K,IA,1,MGAT,'F','array posn',IER)
            IATDF(IFOC,J)=IA
            call exgmat(ia,SALT,ier)
            TMAT(IFOC,IA)=SALT(1)(1:16)
          ENDIF
        ELSEIF(WORD(1:4).EQ.'INTG')THEN
          ITDFTR(IFOC,J)=3
          CALL EGETWI(GMF(J),K,IA,1,MGAI,'F','array posn',IER)
          IATDF(IFOC,J)=IA
          CALL EGETWI(GMF(J),K,IV,0,0,'-','default',IER)
          ITMAR(IFOC,IA)=IV
          CALL EGETWI(GMF(J),K,IV,0,0,'-','min ',IER)
          ITMIN(IFOC,IA)=IV
          CALL EGETWI(GMF(J),K,IV,0,0,'-','max ',IER)
          ITMAX(IFOC,IA)=IV
          IF(IER.NE.0)RETURN
        ENDIF
   6  CONTINUE

      RETURN
      END
