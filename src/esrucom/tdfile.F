C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C tdfile.f - Temporal file I/O facilities for ESP-r modules.
C Current version February 2005.

C Contents:
C Supplyandcheck - where user supplies an existing temporal file (tfile)
C       and conversions are made depending on the calling module. 
C ERTDF reads an binary/scratch) TDF file header and returns info
C       into common blocks. 
C EWTDF writes a TDF scratch file from info in common blocks.
C EWTDFA writes an ascii TDF file from info in common blocks.
C TDFAIMPT imports a version 2/3 ascii TDF file into commons. 
C ERTABU reads one timestep data from scratch file.
C EWTABU writes one timesteps Tabular info to TDF scratch file. 
C EWTABA exports one timesteps Tabular info to a text file.
C CLRTAB clears the current working tabular timestep data.

C Notes:
C Keep tdf file info in LTDF,LTDFA, unit=IUTDF, IUTDFA

C ************* supplyandcheck
C Supplyandcheck - where user supplies an existing temporal file (tfile)
C and conversions are made depending on the calling module.
C The parameter act indicates the module that called supplyandcheck 
C as follows:
C  act = `P` project manager which only needs the information with
C            the header of the ascii version. If older binary file
C            it will convert it to ascii.
C  act = `T` tdf which can deal with any version of the file as well
C            as ascii and binary. It will convert older binary to ascii
C            with a scratch file. It will create a scratch file for
C            ascii versions supplied.
C  act = `S` simulator which will build scratch file from the ascii
C            version of the file and instanciate the tdfflg2 commons.
C            If non-asci temporal supplied warn and return.
C  act = `R` res which will build scratch file from the ascii
C            version of the file as well as instanciating the tdfflg2
C            commons.

C  Check whether it is an ascii file or a binary file. Depending on
C which create the scratch or ascii version.
      subroutine supplyandcheck(tfile,act,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"
C Related to TDF ITEMS file.
#include "control.h"      
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRC/ITRC
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)

C Need to loop for number of surfaces for bidirectional and vertical solar radiation data

      COMMON/TDFOPEN/OPTDF
      LOGICAL OPTDF,XST

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      character*12 ndnam

      CHARACTER tfile*72,LTMPFL*72
      character FOCUS*8,msgl2*48,fs*1,act*1,outs*124
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      CHARACTER HDR*12
      logical unixok,FOUND
      integer ier

C First check if tfile is an ascii temporal file.
      IER=0
      FOCUS='ALL'
      FOUND=.false.
      CALL GENDAT(FOCUS,0,FOUND,IER)

C Use TDFAIMPT with check option so most of the header is scanned.
      call TDFAIMPT(ITRC,tfile,IUTDFA,'c',IER)
      CALL ERPFREE(IUTDFA,ISTAT)
      if(ier.eq.0.and.NITDF.ge.1)then
        LTDFA=tfile

C For all actions except 'T' check that the data in the configuration
C file are consistent with the zone names (this is to complete syntax
C checking of associations between the zones/controls and temporal file.
C Save a zone based pointer back to the tdf item as well. Note: a model
C configuration can can have several references to the same temporal entity.
        if(act.eq.'T'.or.act.eq.'f')then
          continue
        else
          if(ITEMSTD.gt.0)then
            NDATA=NITDF
            do 78 itd1=1,ITEMSTD
              do 79 itd3=1,ncomp
                if(DBZN(itd1)(1:12).eq.zname(itd3)(1:12))then

C Search instances in temporal file to find match(s).
                  DO 77 itd4=1,NDATA
                    if(TAG(itd4)(1:lnblnk(TAG(itd4))).EQ.
     &                 DBTAG(itd1)(1:lnblnk(DBTAG(itd1))))then
                      if(DBTASK(ITD1)(1:7).eq.'CASUALT')
     &                  ICASUAL(itd3)=itd4
                      if(DBTASK(ITD1)(1:7).eq.'CASUAL3')
     &                  ICASUAL3(itd3)=itd4
                      if(DBTASK(ITD1)(1:6).eq.'ZIVENT')
     &                  IZIVENT(itd3)=itd4
                      if(DBTASK(ITD1)(1:6).eq.'RAIRVL')
     &                  IRAIRVL(itd3)=itd4
                      if(DBTASK(ITD1)(1:6).eq.'CTLSTA')
     &                  ICTLSTA(itd3)=itd4
                      if(DBTASK(ITD1)(1:8).eq.'ACTIVITY')
     &                  IACTIV(itd3)=itd4
                      if(DBTASK(ITD1)(1:8).eq.'HTCLSETP')
     &                  IHTCLSETP(itd3)=itd4
                      if(DBTASK(ITD1)(1:6).eq.'DBTZNM')
     &                  IDBTZNM(itd3)=itd4   ! measured zn dbT

C Bidirectional data are assigned per surface.
                      if(DBTASK(ITD1)(1:8).eq.'BIDIRSET')then
                        DO 75 IS=1,NZSUR(itd3)
                          if(DBSN(itd1)(1:12).eq.
     &                       sname(itd3,IS)(1:12))then
                            IBIDIRS(itd3,IS)=itd4
                          endif
  75                    continue
                      endif

C Vertical solar radiation data (used in solar.F) are assigned per surface.
                      if(DBTASK(ITD1)(1:7).eq.'VERTSOL')then
                        DO 975 IS=1,NZSUR(itd3)
                          if(DBSN(itd1)(1:12).eq.
     &                       sname(itd3,IS)(1:12))then
                            IVERSOL(itd3,IS)=itd4
                          endif
 975                    continue
                      endif

C Measured surface temperature data to display in res are assigned
C per surface.
                      if(DBTASK(ITD1)(1:5).eq.'SURTM')then
                        DO 976 IS=1,NZSUR(itd3)
                          if(DBSN(itd1)(1:12).eq.
     &                       sname(itd3,IS)(1:12))then
                            ISURTMEAS(itd3,IS)=itd4
                          endif
 976                    continue
                      endif
                    endif
  77              continue
                endif
  79          continue

C Check if fluid mass flow network pressures are to be assigned
              if(DBTASK(itd1)(1:8).eq.'NODPRESS')then
                do 980 inod=1,nnod
                  iln=lnblnk(ndnam(inod))
                  if(ndnam(inod)(1:iln).eq.dbzn(itd1)(1:iln))then
                    do 981 itd4=1,ndata
                      iln=lnblnk(tag(itd4))
                      if(tag(itd4)(1:iln).eq.dbtag(itd1)(1:iln))then
                        IMFNTDFP(inod)= itd4
                        IMFNTDFF=1
                      endif
 981                continue
                  endif
 980            continue
              endif

C Check if temporal item association is to a control loop. Loop through each
C control function to see if multiple loops reference the same tdf item.
C << note that isetptt should be dimensioned MCF >>
              ij=0
              if(DBZN(itd1)(1:5).eq.'loop_')then
                read(DBZN(itd1)(6:7),*,iostat=ios,err=2)ij
                do 80 itd3=1,mcf
                  if(ij.eq.itd3)then
                    if(DBTASK(ITD1)(1:6).eq.'SETPTT')then

C Search instances in temporal file to find match(s).
                      DO 81 itd4=1,NDATA
                        IF(TAG(itd4)(1:lnblnk(TAG(itd4))).EQ.
     &                    DBTAG(itd1)(1:lnblnk(DBTAG(itd1))))THEN
                          ISETPTT(itd3)=itd4
                        ENDIF
  81                  CONTINUE
                    endif
                  endif
  80            continue
              endif          
  78        continue

C Debug...
C            write(6,*)'ISETPTT',ISETPTT
C            write(6,*)'ICASUAL',ICASUAL
C            write(6,*)'ICASUAL3',ICASUAL3
C            write(6,*)'ICTLSTA',ICTLSTA

          endif
        endif

        if(act.eq.'P'.or.act.eq.'p')then

C If act=`P` (project manager) then the header common blocks have
C been scanned and there is no need for the scratch file.
          return
        else

C This if for the simulator or tdf or res. We have been passed an 
C ascii temporal file. Create a name for the binary scratch file
C and the call TDFAIMPT with 'i' import option to create that
C scratch file. Open it wide enought for the current data or
C 20 +1 words wide.
          call isunix(unixok)
          if(unixok)then
            fs = char(47)
            call esppid(ipid)
            LTMPFL=' '
            write(LTMPFL,'(2a,i7,a)') '/tmp',fs,ipid,
     &        '.temporalscratch'
          else
            fs = char(92)
            call esppid(ipid)
            LTMPFL=' '
            write(LTMPFL,'(4a,i7,a)') 'C:',fs,'temp',fs,ipid,
     &        '.temporalscratch'
          endif
          call st2file(LTMPFL,LTDF)

C The call with an 'i' action will have created the scratch file
C of a minimal width.
          call TDFAIMPT(ITRC,LTDFA,IUTDFA,'i',IER)
          msgl2=' '
          CALL USRMSG('Converting temporal data...done.',msgl2,'P')

C Now open the scratch file with the proper record width.

C Debug.
C          write(6,*) 'after converting ',nwpr,nuwpr

          if(act.eq.'S'.or.act.eq.'s'.or.act.eq.'R'.or.act.eq.'r')then

C setup the TDFFLG2 common blocks based on TDFFLG0 commons specified
C in the configuration file if called from simulator or res.
            IALLCLM=0
            ISKYLUX=0
            IDBTEXT=0
            IWINDVL=0
            IWINDDR=0
            IRELHUM=0
            IDIFHSL=0
            IDIRSOL=0
            IGLOHSL=0
            IOBJVEL=0
            IOBJDIR=0
            ISKYTMP=0
            IGLOVRT=0

C Search instances in temporal file to find match(s).
            NDATA=NITDF
            do 4 M=1,ITEMSTD
              FOUND=.FALSE.
              DO 3 I=1,NDATA
                IF(TAG(I)(1:lnblnk(TAG(I))).EQ.
     &             DBTAG(M)(1:lnblnk(DBTAG(M))))THEN

C Debug.
C                  write(6,*) ' i tag m dbtag ',i,tag(i),m,dbtag(m)

                  FOUND=.TRUE.
                  IFOC=I
                ENDIF
    3         CONTINUE
              IF(.NOT.FOUND)THEN
                write(outs,'(2A)')DBTAG(M),' not in temporal file.'
                CALL USRMSG(' ',outs,'W')
                CALL ERPFREE(IUTDF,ISTAT)
                IER=1
                RETURN
              ENDIF

C Assign pointer from system use of a tdf entry to the entry index
C in the tdf file.
              if(TTYPE(ifoc)(1:6).EQ.'ALLCLM')then
                IALLCLM=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'DBTEXT')then
                IDBTEXT=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'SKYLUX')then
                ISKYLUX=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'WINDVL')then
                IWINDVL=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'WINDDR')then
                IWINDDR=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'RELHUM')then
                IRELHUM=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'DIFHSL')then
                IDIFHSL=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'GLOHSL')then
                IGLOHSL=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'DIRSOL')then
                IDIRSOL=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'SKYTMP')then
                ISKYTMP=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'GLOVRT')then
                IGLOVRT=ifoc
              elseif(TTYPE(ifoc)(1:6).EQ.'OBJROT')then
                IOBJDIR=ifoc
              endif
              WRITE(OUTS,'(A,I2,7A,I2,A)')
     &            ' Tdf item:',M,' :',DBTAG(M),
     &            ' of type: ',TTYPE(ifoc),
     &            ' associated with ',DBZN(M),
     &            ' temporal index ',IFOC,' referenced.'
              IF(ITRC.GE.1)CALL EDISP(iuout,OUTS)
  4         continue
            WRITE(OUTS,'(A,9I2)')' Simul tags:',IALLCLM,IDBTEXT,IWINDVL,
     &        IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IGLOVRT
            IF(ITRC.GE.1)CALL EDISP(iuout,outs)
            IF(ITRC.GE.1)CALL EDISP(iuout,' ')
          endif
        endif
      else

C Failed opening ascii temporal file so try binary version.
C If act is `S` then warn the user to use tdf first to convert.
        if(act.eq.'S'.or.act.eq.'s')then
          call usrmsg('Expecting ascii temporal file. Use prj or tdf',
     &      'to convert binary temporal file','W')
          return
        endif

C If act is `R` then warn the user to use tdf first to convert. 
        if(act.eq.'R'.or.act.eq.'r')then
          call usrmsg('Expecting ascii temporal file. Use prj or tdf',
     &      'to convert binary temporal file','W')
          return
        endif

C Otherwise convert to asci file by first reading in the binary file.
        OPTDF=.FALSE.
        CALL ERPFREE(iutdf,ISTAT)
        call findfil(tfile,XST)
        IF(.NOT.XST)THEN
          msgl2='(binary/scratch format)'
          CALL USRMSG(' TDF file not found...',msgl2,'-')
          ier=2
          return
        ENDIF

C Initially set file width to one more than maximum number of columns.
        NWPR=MTABC
        ITWPR=NWPR+1
        ier=0
        call EFOPRAN(iutdf,tfile,ITWPR,1,IER)
        IF(ier.ne.0)THEN
          WRITE(OUTS,'(2A)')'Problem opening ',tfile(1:lnblnk(tfile))
          CALL EDISP(IUOUT,OUTS)
          IER=1
          RETURN
        ELSE

C Test if this is the proper record width via seeing what NWPR is.
          IREC=1
          msgl2='(test read of record 1 to get width)'
          READ(iutdf,REC=IREC,IOSTAT=ISTAT,ERR=102)HDR,NWPR
          if(NWPR+1.eq.ITWPR)then
            OPTDF=.TRUE.
          else
            close(iutdf)
            ITWPR=NWPR+1
            ier=0
            call EFOPRAN(IUTDF,tfile,ITWPR,3,IER)
            IF(ier.ne.0)THEN
              WRITE(OUTS,'(A,A)')' problem opening ',
     &          tfile(1:lnblnk(tfile))
              CALL EDISP(IUOUT,OUTS)
              IER=1
              RETURN
            else
              OPTDF=.TRUE.
           endif
          endif

C Scan the TDF binary file, then read in each of the instances.
          LTDF=tfile
          IFOC=0
          CALL ERTDF(ITRC,IFOC,IER)
          if(IER.NE.0)then
            WRITE(OUTS,'(A,A)')' Problem in header of ',
     &        LTDF(1:lnblnk(LTDF))
            CALL EDISP(IUOUT,OUTS)
            IER=1
            RETURN
          endif
          if(NITDF.gt.0)then
            DO 44 L=1,NITDF
              LL=L
              CALL ERTDF(ITRC,LL,IER)
   44       CONTINUE
            if(IER.NE.0)then
              WRITE(OUTS,'(A,A)')' Problem in an item of ',
     &          LTDF(1:lnblnk(LTDF))
              CALL EDISP(IUOUT,OUTS)
            endif
          endif
        ENDIF

        msgl2=' Scanning supplied file...done.'
        call usrmsg(' ',msgl2,'P')
        msgl2='  '
        call usrmsg(' ',msgl2,'-')

C Write out ascii version of the file. Append an 'a' to the
C end of the file if it ends with 'tdf' otherwise append '.a'.
        lntdf=lnblnk(LTDF)
        LTDFA=' '
        if(LTDF(lntdf-3:lntdf).eq.'.tdf')then
          write(ltdfa,'(2a)') LTDF(1:lntdf),'a'
        else
          write(ltdfa,'(2a)') LTDF(1:lntdf),'.a'
        endif
        call usrmsg(' ','Updating the ascii file...','-')
        call ewtdfa(itrc,ltdfa,iutdfa,0,ier)
        if(ier.eq.0)then
          write(msgl2,'(2a)') ltdfa(1:lnblnk(ltdfa)),
     &      ' with your model.'
          call usrmsg(
     &      'An ascii version has been created & you should use',
     &      msgl2,'W')
        endif
      endif

      return

C Binary file read errors.
 102  CALL USRMSG(' could not read header record 1',msgl2,'W')
      IER=1
      return

  2   msgl2='  '
      CALL USRMSG('Problem associating tdf item with contrl.',msgl2,'W')
      IER=1
      return
      end

C ************* ERTDF 
C ERTDF reads a binary/scratch TDF file and returns header info into
C common blocks. If IFOC=0 then scan initial records, else if FOC>0
C then return detailed header info on that instance only. This is
C called by supply and check in cases where the user has provided
C an binary, rather than an asci file to read. 
C ITRC is the trace level, IER=0 OK, IER=1 problem (including that
C the file is an asci or unsupported version). 

C If the version is 1 or 2 then details of the fields of each item
C will be read in. If version 3 then only a limited header is expected.
      SUBROUTINE ERTDF(ITRC,IFOC,IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN

C Pointer to start of each item header in TDF file.
      COMMON/HDAT/IHREC(MIT)
      common/tdaid/tdaide(2)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

      CHARACTER WORD*8
      character tdbdate*24  ! date stamp for temporal scratch file
      CHARACTER FSTR*80,HDR*12,OUTS*124,tdaide*72,msgl2*48

C Assume that the file is already opened before calling this routine.
      IER=0

C Read the header information. 3+1+1+1+1+1+1+1+1+1+6+1 (19)
      IREC=1
      msgl2='ERTDF:(scan of 1st record)'
      READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)HDR,NWPR,NITDF,
     &  NTSPH,itdyear,itdbdoy,itdedoy,NEXTRC,NEXTCL,NDBSTP,tdbdate

      IF(IFOC.EQ.0)THEN
        if(NWPR.lt.MTABC)then
          write(outs,'(a,i2,a,i2)') 'Current columns (',NWPR,
     &      ') are < ',MTABC
          call edisp(iuout,outs)
        endif
      endif

C The last record of the binary file is 50 + NDBSTP
      LSTREC=50+NDBSTP
      if(HDR(1:12).eq.'TDFdatabase2')THEN
        if(ifoc.eq.0)NUWPR=0
        ITDFLG=2
      elseif(HDR(1:12).eq.'TDFdatabase3')THEN

C Rescan the first record to get number of columns used.
        IREC=1
        msgl2='ERTDF:(rescan of 1st record)'
        READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)HDR,NWPR,NITDF,
     &    NTSPH,itdyear,itdbdoy,itdedoy,NEXTRC,NEXTCL,NDBSTP,tdbdate,
     &    NUWPR
        ITDFLG=3
      elseif(HDR(1:12).eq.'TDF database')THEN
        CALL EDISP(IUOUT,' File is an unsupported version of temporal!')
        IER=1
        RETURN
      elseif(HDR(1:9).eq.'ASCIITDF3')THEN
        CALL EDISP(IUOUT,' File is an ASCII version of a temporal!')
        IER=1
        RETURN
      elseif(HDR(1:9).eq.'ASCIITDF2')THEN
        CALL EDISP(IUOUT,' File is an ASCII version of a temporal!')
        IER=1
        RETURN
      else
        CALL EDISP(IUOUT,' This is not a temporal file!')
        IER=1
        RETURN
      ENDIF
      IF(ITRC.GT.1.AND.IFOC.LE.1)THEN
        CALL EDISP(IUOUT,' Reading TDF file header')
        CALL EDISP(IUOUT,
     &' nwpr,nitdf,ntsph,itdyear,itdbdoy,itdedoy,nextrc,nextcl,ndbstp')
        WRITE(OUTS,'(4I5,3X,7I6)')NWPR,NITDF,NTSPH,itdyear,itdbdoy,
     &                      itdedoy,NEXTRC,NEXTCL,NDBSTP,NUWPR
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,tdbdate)
      ENDIF

      IREC=2
      msgl2='ERTDF:(rescan of 2nd record)'
      READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)tdaide(1)

      IREC=3
      msgl2='ERTDF:(rescan of 3rd record)'
      READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)tdaide(2)
      IF(ITRC.GT.1)THEN
        WRITE(OUTS,'(2A)') '1st desc: ',tdaide(1)
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(2A)') '2nd desc: ',tdaide(2)
        CALL EDISP(IUOUT,OUTS)
      ENDIF

C Record which points to the beginning of each instance header.
      IREC=4
      NDATA=NITDF
      msgl2='ERTDF:(rescan of 4th record ihrec)'
      READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)(IHREC(K),K=1,NDATA)
      I=0
  100 CONTINUE
      IF(IFOC.EQ.0)THEN

C General scan. When all ndata items have been scanned then
C read data in records 48 and 49.
        I=I+1
        IREC=IHREC(I)
        IF(IREC.EQ.0.OR.I.GT.NDATA)then

C Now read the maximum/minimum for each data column.
          msgl2='ERTDF:(rescan of 48th record ihrec)'
          IREC=48
          READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=104)(TABUMX(J),J=1,NUWPR)
          msgl2='ERTDF:(rescan of 49th record ihrec)'
          IREC=49
          READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=104)(TABUMN(J),J=1,NUWPR)
          RETURN
        endif
      ELSEIF(IFOC.NE.0)THEN
        I=IFOC
        IREC=IHREC(I)
        IF(IREC.EQ.0.OR.I.GT.NDATA)RETURN
      ENDIF
        
C Read instance attributes. 3+2+4+9+1+1 (20)
      msgl2='ERTDF:(scan of attributes)'
      READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103)TAG(I),TTYPE(I),
     &  TMENU(I),TAIDE(I),NTSTAT(I),NTBITS(I)
      IF(ITRC.GT.1)THEN
        CALL EDISP(IUOUT,' ')
        WRITE(OUTS,'(A,A,A,I3)') 'tag : ',TAG(I),' @ record:',IREC
        CALL EDISP(IUOUT,OUTS)
        WRITE(OUTS,'(4A)')    'type: ',TTYPE(I),' descr: ',TAIDE(I)
        CALL EDISP(IUOUT,OUTS)
      ENDIF

C Increment NUWPR (count of columns used) if we are scanning the
C full file version 2 (if fococ is zero).
      if(HDR(1:12).eq.'TDFdatabase2')THEN
        IF(IFOC.EQ.0) NUWPR = NUWPR + NTBITS(I)
      ENDIF

C For each of the fields read strings which will hold the data.
C This is done so that the ordering of the fields and their variant 
C record structures can be accommodated. If scanning file don't bother
C with reading these records.
      IF(IFOC.EQ.0)GOTO 100

C If version 3 do not bother reading the fields because this information
C is contained in the ascii temporal file header.
      if(ITDFLG.eq.3)then
        continue
      else
        IFIELD=NTSTAT(I)+NTBITS(I)
        NTMAR(I)=0
        NTMAT(I)=0      
        NTMAI(I)=0      
        DO 60 J=1,IFIELD
          msgl2='ERTDF:(scan of fstr attributes)'
          IREC=IREC+1
          READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103)FSTR
          IF(ITRC.GT.1)THEN
            WRITE(OUTS,'(A,I2,A,I3,1X,A)') 'fld : ',J,' rec:',IREC,FSTR
            CALL EDISP(IUOUT,OUTS)
          ENDIF

C Strip data from FSTR (80 char).
          IF(FSTR(1:4).EQ.'REAL')THEN
            K=0
            CALL EGETW(FSTR,K,WORD,'W','REAL',IFLAG)
            CALL EGETWI(FSTR,K,IA,0,0,'-','IA',IER)
            CALL EGETWI(FSTR,K,IC,0,0,'-','IC',IER)
            CALL EGETWR(FSTR,K,TMAR(I,IA),0.0,0.0,'-','TMAR(I,IA)',IER)
            CALL EGETWR(FSTR,K,TMIN(I,IA),0.0,0.0,'-','TMIN(I,IA)',IER)
            CALL EGETWR(FSTR,K,TMAX(I,IA),0.0,0.0,'-','TMAX(I,IA)',IER)
            CALL EGETRM(FSTR,K,TDFDES(I,J),'W','TDFDES(I,J)',IER)
            IATDF(I,J)=IA
            ITCOL(I,J)=IC
            ITDFTR(I,J)=1
            NTMAR(I)=NTMAR(I)+1
          ELSEIF(FSTR(1:4).EQ.'TEXT')THEN
            K=0
            CALL EGETW(FSTR,K,WORD,'W','TEXT',IFLAG)
            CALL EGETWI(FSTR,K,IA,0,0,'-','IA',IER)
            CALL EGETWI(FSTR,K,IC,0,0,'-','IC',IER)
            CALL EGETW(FSTR,K,TMAT(I,IA),'W','TMAT(I,IA)',IFLAG)
            CALL EGETRM(FSTR,K,TDFDES(I,J),'W','TDFDES(I,J)',IER)
            IATDF(I,J)=IA
            ITCOL(I,J)=IC
            ITDFTR(I,J)=2
            NTMAT(I)=NTMAT(I)+1
          ELSEIF(FSTR(1:4).EQ.'INTG')THEN
            K=0
            CALL EGETW(FSTR,K,WORD,'W','INTG',IFLAG)
            CALL EGETWI(FSTR,K,IA,0,0,'-','IA',IER)
            CALL EGETWI(FSTR,K,IC,0,0,'-','IC',IER)
            CALL EGETWI(FSTR,K,ITMAR(I,IA),0,0,'-','ITMAR(I,IA)',IER)
            CALL EGETWI(FSTR,K,ITMIN(I,IA),0,0,'-','ITMIN(I,IA)',IER)
            CALL EGETWI(FSTR,K,ITMAX(I,IA),0,0,'-','ITMAX(I,IA)',IER)
            CALL EGETRM(FSTR,K,TDFDES(I,J),'W','TDFDES(I,J)',IER)
            IATDF(I,J)=IA
            ITCOL(I,J)=IC
            ITDFTR(I,J)=3
            NTMAI(I)=NTMAT(I)+1
          ENDIF
  60    CONTINUE
      endif

  99  RETURN

C Binary file read errors.
 102  CALL USRMSG(' could not read header record 1',msgl2,'W')
      IER=1
      goto 99
 103  msgl2='  '
      CALL USRMSG(' could not read item record ',msgl2,'W')
      IER=1
      goto 99
 104  msgl2='  '
      CALL USRMSG(' could not read data ranges record ',msgl2,'W')
      IER=1
      goto 99
      END

C ************* EWTDF 
C EWTDF writes an binary (scratch) TDF file from info in common blocks.
C IER=0 OK, IER=1 problem. 
      SUBROUTINE EWTDF(IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/HDAT/IHREC(MIT)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      common/tdaid/tdaide(2)
      COMMON/TDFOPEN/OPTDF
      LOGICAL OPTDF

      CHARACTER FSTR*80,HDR*12,tdaide*72
      CHARACTER msgl2*48
      CHARACTER tdbdate*24  ! data stamp for temporal scratch file
      character tail*8
      integer ier

C Write out to version 3 format. This assumes that the binary
C file will be used as a scratch file for the tdf module as
C well as for the project manager, simulator and results analysis
C and that the ASCII version of the file will be held with the
C model.
      HDR  ='TDFdatabase3'
      call dstamp(tdbdate)

C If not already opened free unit and check if file exists.
      IER=0
      IF(.NOT.OPTDF)THEN
        CALL ERPFREE(IUTDF,ISTAT)
        ITWPR=NWPR+1
        ier=0
        call EFOPRAN(IUTDF,LTDF,ITWPR,3,IER)
        IF(ier.ne.0)THEN
          msgl2=' error creating BINARY TDF file.'
          CALL USRMSG(' ',msgl2,'W')
          IER=1
          RETURN
        ELSE
          OPTDF=.TRUE.
        ENDIF
      ENDIF

C Free up pointer to start of each item header.
      NDATA=NITDF
      DO 45 IIT=1,NWPR
        IHREC(IIT)=0
   45 CONTINUE

C Write the header information.
      IREC=1
      msgl2='scratch record 1 '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)HDR,NWPR,NITDF,
     &  NTSPH,itdyear,itdbdoy,itdedoy,NEXTRC,NEXTCL,NDBSTP,tdbdate,
     &  NUWPR
      IREC=2
      msgl2='scratch record 2 '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)tdaide(1)

      IREC=3
      msgl2='scratch record 3 '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)tdaide(2)

C Record which points to the beginning of each item header.
      IREC=4
      msgl2='scratch record 4 '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)(IHREC(K),K=1,NWPR)

C For each item read its attributes.3+2+8+9+1+1
      if(NDATA.eq.0) goto 11
      DO 10 I=1,NDATA
        IREC=IREC+1
        IHREC(I)=IREC
        msgl2='scratch item tag & type '

        WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103)TAG(I),TTYPE(I),
     &    TMENU(I),TAIDE(I),NTSTAT(I),NTBITS(I)

        IFIELD=NTSTAT(I)+NTBITS(I)

C If version 3, there is no need to write out the fields of each item.
        if(HDR(1:12).eq.'TDFdatabase3')then
          goto 10
        else

C For each of the fields build up text strings which will hold the data.
C This is done so that the ordering of the fields and their variant 
C record structures can be accommodated.
          DO 60 J=1,IFIELD
            IA=IATDF(I,J)
            IC=ITCOL(I,J)
            IF(ITDFTR(I,J).EQ.1)THEN
              WRITE(FSTR,62)IA,IC,TMAR(I,IA),TMIN(I,IA),
     &                      TMAX(I,IA),TDFDES(I,J)
   62         FORMAT('REAL',2I3,3F11.2,2X,A32)
            ELSEIF(ITDFTR(I,J).EQ.2)THEN
              WRITE(FSTR,63)IA,IC,TMAT(I,IA),TDFDES(I,J)
   63         FORMAT('TEXT',2I3,2X,A16,2X,A32)
            ELSEIF(ITDFTR(I,J).EQ.3)THEN
              WRITE(FSTR,64)IA,IC,ITMAR(I,IA),ITMIN(I,IA),
     &                      ITMAX(I,IA),TDFDES(I,J)
   64         FORMAT('INTG',2I3,3I10,2X,A32)
            ENDIF

            IREC=IREC+1
            msgl2='record holding fstr '
            WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103)FSTR
  60      CONTINUE
        endif
  10  CONTINUE

C Remember the last record used by the header section.
  11  LASTHD=IREC

C Go back and update the pointer to item header records.
      IREC=4
      msgl2='scratch ihrec update '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)(IHREC(K),K=1,NWPR)

C Now write the maximum for each column and then the last file record.
      IREC=48
      msgl2='scratch tabumx rec 48 '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102) (TABUMX(J),J=1,NWPR)
      IREC=49
      msgl2='scratch tabumn rec 49 '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102) (TABUMN(J),J=1,NWPR)
      IREC=50+NDBSTP
      tail='ENDTDF  '
      msgl2='scratch tail record '
      WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102) tail
  99  RETURN

C Binary file read errors.
 102  CALL USRMSG(' could not write TDF header record 1',msgl2,'W')
      IER=1
      goto 99
 103  CALL USRMSG(' could not write TDF instance record ',msgl2,'W')
      IER=1
      goto 99

      END

C ************* EWTDFA 
C EWTDFA writes an ascii TDF file (AFIL) on unit IAFIL, from info in
C common blocks and scanning the binary scratch file for timestep data.
C ITRC is the trace level, IER=0 OK, IER=1 problem.
C iadcols is used to adjust the number of columns of data to
C   be written (i.e. nuwpr+iadcols). If iadcols is positive then
C   additional columns of default data will be written. If iadcols is
C   negative then fewer columns will be written.
C   If nuwpr+iadcols > nwpr then a modified nwpr is written out.
      SUBROUTINE EWTDFA(ITRC,AFIL,IAFIL,iiadcols,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
      
      integer lnblnk  ! function definition

      COMMON/HDAT/IHREC(MIT)
      common/tdaid/tdaide(2)
      common/TDFADDC/iadcols,deftabu(9)

      CHARACTER AFIL*72,FSTR*80,HDR*12,tdaide*72
      CHARACTER delim*1
      CHARACTER tab*1,msgl2*72,louts*124

      tab=','
      HDR  ='ASCIITDF3'

C If parameter iiadcols pass with the routine call is the same as that
C set in the common blocks (as a result of adding or deleting items then
C continue. If iadcols is zero (i.e. nothing added or deleted) and
C iiadcols is non-zero (directive to compact ascii file) then reset
C iadcols.
      if(iiadcols.eq.iadcols)then
        continue
      else

C Debug.
C        write(6,*) 'iiadcols is ',iiadcols,' iadcols is',iadcols

        if(iadcols.eq.0.and.iiadcols.ne.0) iadcols = iiadcols
      endif

C If not already opened free unit and check if file exists.
      IER=0
      CALL ERPFREE(iafil,ISTAT)
      CALL EFOPSEQ(iafil,AFIL,4,IER)
      IF(ier.ne.0)THEN
        write(msgl2,'(3a)') 'temporal information ',AFIL(1:24),'.'
        CALL USRMSG(
     &    'There was an error creating the file which holds the',
     &    msgl2,'W')
        IER=1
        RETURN
      ENDIF

C The scratch file will have already defined ihrec.
      NDATA=NITDF

C Write the header information.
      WRITE(iafil,'(a)',IOSTAT=ios,ERR=101)HDR
      WRITE(iafil,'(2a)',IOSTAT=ios,ERR=101)'# NWPR NITDF NTSPH',
     &  ' itdyear,itdbdoy,itdedoy,columns'
      if(nuwpr+iadcols.gt.nwpr)then

C If nuwpr+iadcols > nwpr then a modified nwpr is written out.
        WRITE(iafil,'(8I5)',IOSTAT=ios,ERR=101)nuwpr+iadcols,NITDF,
     &    NTSPH,itdyear,itdbdoy,itdedoy,nuwpr+iadcols
      elseif(nuwpr+iadcols.lt.nwpr)then

C If fewer columns needed, check that nwpr does not go below 20.
        if(nuwpr+iadcols.ge.20)then
          WRITE(iafil,'(8I5)',IOSTAT=ios,ERR=101)nuwpr+iadcols,NITDF,
     &      NTSPH,itdyear,itdbdoy,itdedoy,nuwpr+iadcols
        else
          WRITE(iafil,'(a,7I5)',IOSTAT=ios,ERR=101) '  20 ',NITDF,
     &      NTSPH,itdyear,itdbdoy,itdedoy,nuwpr+iadcols
        endif
      else

C We have same number of columns.
        WRITE(iafil,'(8I5)',IOSTAT=ios,ERR=101)NWPR,NITDF,
     &    NTSPH,itdyear,itdbdoy,itdedoy,NUWPR
      endif
      WRITE(iafil,'(a)',IOSTAT=ios,ERR=101)
     &  '# NEXTRC,NEXTCL,NDBSTP'
      WRITE(iafil,'(3i7)',IOSTAT=ios,ERR=101)NEXTRC,NEXTCL,NDBSTP
      WRITE(iafil,'(3a)',IOSTAT=ios,ERR=101)'*tdaid1',tab,
     &  tdaide(1)(1:lnblnk(tdaide(1)))
      WRITE(iafil,'(3a)',IOSTAT=ios,ERR=101)'*tdaid2',tab,
     &  tdaide(2)(1:lnblnk(tdaide(2)))

C For each item write its attributes.
      DO 10 I=1,NDATA
        WRITE(iafil,'(A)',IOSTAT=ios,ERR=102)'*items'
        WRITE(iafil,'(3a)',IOSTAT=ios,ERR=103)'*tag',tab,TAG(I)
        WRITE(iafil,'(3a)',IOSTAT=ios,ERR=103)'*type',tab,TTYPE(I)
        WRITE(iafil,'(3a)',IOSTAT=ios,ERR=103)'*menu',tab,TMENU(I)
        WRITE(iafil,'(3a)',IOSTAT=ios,ERR=103)'*aide',tab,TAIDE(I)
        WRITE(iafil,'(a,a,2i4)',IOSTAT=ios,ERR=103)'*other',tab,
     &    NTSTAT(I),NTBITS(I)

        IFIELD=NTSTAT(I)+NTBITS(I)
        WRITE(iafil,'(2a,i2)',IOSTAT=ios,ERR=103)'*fields',tab,ifield

C For each of the fields build up text strings which will hold the data.
C This is done so that the ordering of the fields and their variant 
C record structures can be accommodated.
        DO 60 J=1,IFIELD
          IA=IATDF(I,J)
          IC=ITCOL(I,J)
          IF(ITDFTR(I,J).EQ.1)THEN
            WRITE(FSTR,62)IA,IC,TMAR(I,IA),TMIN(I,IA),
     &                    TMAX(I,IA),TDFDES(I,J)
   62       FORMAT('REAL',2I3,3F11.3,2X,A32)
          ELSEIF(ITDFTR(I,J).EQ.2)THEN
            WRITE(FSTR,63)IA,IC,TMAT(I,IA),TDFDES(I,J)
   63       FORMAT('TEXT',2I3,2X,A16,2X,A32)
          ELSEIF(ITDFTR(I,J).EQ.3)THEN
            WRITE(FSTR,64)IA,IC,ITMAR(I,IA),ITMIN(I,IA),
     &                    ITMAX(I,IA),TDFDES(I,J)
   64       FORMAT('INTG',2I3,3I10,2X,A32)
          ENDIF
          WRITE(iafil,'(a)',IOSTAT=ios,ERR=103)FSTR
  60    CONTINUE
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=103)'*end_item'
  10  CONTINUE

C Record which points to the beginning of each item header.
      WRITE(iafil,'(A)',IOSTAT=ios,ERR=102)'*pointers'
      if(NDATA.eq.1)then
        WRITE(iafil,'(1x,i3)',IOSTAT=ios,ERR=102) ihrec(1)
      else
        itrunc=1
        ipos=1
        delim='C'
        do while (itrunc.ne.0)
          call ailist(ipos,NDATA,IHREC,MIT,delim,louts,loutln,itrunc)
          WRITE(iafil,'(1x,a)',IOSTAT=ios,ERR=102) louts(1:loutln)
          ipos=itrunc+1
        end do
      endif

C Now scan the scratch file for timestep data. If this is the initail
C creation of data the scratch file will be blank so just clear the
C tabular fields.
      IF(NITDF.gt.0)THEN
        DO 46 IP=1,NDBSTP
          ITREC=IP+49
          if(nuwpr.eq.0)then
            CALL CLRTAB
          else
            CALL ERTABU(ITRC,ITREC,IER)
          endif
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))

C If columns need to change iadcol will be non-zero.
          CALL EWTABA(ITREC,RDOTY,iafil,IER)
          IF(IER.EQ.1)RETURN
   46   CONTINUE
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=102) '*end_tabular_data'
      endif
  99  CALL ERPFREE(iafil,ISTAT)
      RETURN

C Export file write errors.
 101  msgl2='  '
      CALL USRMSG('Could not write TDF header',msgl2,'W')
      IER=1
      goto 99
 102  msgl2='  '
      CALL USRMSG('Could not write TDF header line 1',msgl2,'W')
      IER=1
      goto 99
 103  msgl2='  '
      CALL USRMSG('Could not write TDF item data.',msgl2,'W')
      IER=1
      goto 99

      END

C ************* TDFAIMPT
C TDFAIMPT imports (act='i') or checks (act='c') a v2 or v3 ascii TDF file
C into common blocks and into the current binary file LTDF on the currnt
C file unit IUTDF. The checking option restricts itself to the header of
C the file and does not work with the scratch file. This is useful if
C the user only wishes to associate building information with the temporal
C file.
C ITRC is the trace level, IER=0 OK, IER=1 problem.
C When called in 'checking' mode, enough of the common blocks are
C filled to allow a listing of the current temporal items (but not
C their timestep data). 
C Note that a call with act='i' is assumed to happen after a call with
C act='c'.
      SUBROUTINE TDFAIMPT(ITRC,AFIL,IAFIL,act,IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

C espriou.h provides current file.
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TDFOPEN/OPTDF
      COMMON/HDAT/IHREC(MIT)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      common/tdaid/tdaide(2)
      logical close,OPTDF,XST

      CHARACTER*72 AFIL,tdaide
      CHARACTER act*1
      CHARACTER outs*124
      character outstr*248,LOUTSTR*1000,WORD*20,TMP*4,tail*8,msgl2*86
      integer ier
      real toler  ! what is half of the timestep

C If not already opened free unit and check if file exists.
      IER=0
      CALL ERPFREE(iafil,ISTAT)
      CALL EFOPSEQ(iafil,AFIL,1,IER)
      IF(ier.ne.0)THEN
        write(msgl2,'(2a)') ' error opening ',AFIL(1:lnblnk(AFIL))
        CALL USRMSG(' ',msgl2,'W')
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') AFIL(1:lnblnk(AFIL))

C Feedback to the user (in case file is large).
      write(msgl2,'(2a)') ' Scanning...',AFIL(1:lnblnk(AFIL))
      call usrmsg(' ',msgl2,'-')

C Check header of file.
      CALL LSTRIPC(iafil,outstr,99,ND,1,'header',IER)
      IF(IER.NE.0)goto 1
      if(OUTSTR(1:9).eq.'ASCIITDF3')THEN
        ITDFLG=3
      elseif(OUTSTR(1:9).eq.'ASCIITDF2')THEN
        ITDFLG=2
      else
        write(msgl2,'(3a)') 'Not temporal file: HDR is ',outstr(1:12),
     &    ' ( might be binary or old ascii)'
        call edisp(iuout,msgl2)
        if(act.eq.'i')then
          call edisp(iuout,
     &      'If it is supposed to be an ASCII temporal file look for')
          call edisp(iuout,'ASCIITDF3 or ASCIITDF2 on the first line!')
          call edisp(iuout,
     &      'If it ia a binary temporal file it will be read now...')
        endif
        ier=1
        CALL ERPFREE(iafil,ISTAT)
        return
      endif

C If act = 'i' then most of the header will already have been scanned.
C Keep reading until the tag *pointers is found. 
      if(act.eq.'i'.or.act.eq.'I')then
 144    CALL LSTRIPC(iafil,outstr,99,ND,1,'temporal header',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','header tags',IFLAG)
        if(WORD(1:9).eq.'*pointers')then
          goto 145
        else
          goto 144
        endif
      endif

C Read the header information. Newer files will include
C the number of colums actually used (nuwpr). For others
C we have to recover this form ntbits().
      CALL LSTRIPC(iafil,outstr,99,ND,1,'line 1',IER)
      IF(ier.ne.0)THEN
        write(msgl2,'(2a)') ' error on line 1 of ',AFIL(1:lnblnk(AFIL))
        CALL USRMSG(' ',msgl2,'W')
        IER=1
        RETURN
      ENDIF
      K=0
      CALL EGETWI(OUTSTR,K,NWPR,0,0,'-','NWPR',IER)
      CALL EGETWI(OUTSTR,K,NITDF,0,0,'-','NITDF',IER)
      CALL EGETWI(OUTSTR,K,NTSPH,0,0,'-','NTSPH',IER)
      CALL EGETWI(OUTSTR,K,itdyear,0,0,'-','itdyear',IER)
      CALL EGETWI(OUTSTR,K,itdbdoy,0,0,'-','itdbdoy',IER)
      CALL EGETWI(OUTSTR,K,itdedoy,0,0,'-','itdedoy',IER)
      if(ND.eq.7)then
        CALL EGETWI(OUTSTR,K,NUWPR,0,0,'-','NUWPR',IER)
        itnuwpr=0
      else
        NUWPR=NWPR
        itnuwpr=0
      endif

C Free up pointer to start of each item header. Set index for current
C item to 0.
      NDATA=NITDF
      DO 45 IIT=1,NDATA
        IHREC(IIT)=0
   45 CONTINUE
      i=0
      CALL LSTRIPC(iafil,outstr,3,ND,1,'line 2',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NEXTRC,0,0,'-','NEXTRC',IER)
      CALL EGETWI(OUTSTR,K,NEXTCL,0,0,'-','NEXTCL',IER)
      CALL EGETWI(OUTSTR,K,NDBSTP,0,0,'-','NDBSTP',IER)

 34   CALL LSTRIPC(iafil,outstr,0,ND,1,'tags',IER)
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','header tags',IFLAG)
      if(WORD(1:7).eq.'*tdaid1')then
        CALL EGETRM(OUTSTR,K,tdaide(1),'W','tdaide',IER)
        IF(IER.NE.0)goto 1
        goto 34
      elseif(WORD(1:7).eq.'*tdaid2')then
        CALL EGETRM(OUTSTR,K,tdaide(2),'W','tdaide',IER)
        IF(IER.NE.0)goto 1
        goto 34
      elseif(WORD(1:6).eq.'*items')then
        i=i+1
  44    CALL LSTRIPC(iafil,outstr,0,ND,1,'item data',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','item tags',IFLAG)
        if(WORD(1:4).eq.'*tag')then
          CALL EGETRM(OUTSTR,K,TAG(I),'W','*tag',IER)
          IF(IER.NE.0)goto 1
          goto 44
        elseif(WORD(1:5).eq.'*type')then
          CALL EGETRM(OUTSTR,K,TTYPE(I),'W','*type',IER)
          IF(IER.NE.0)goto 1
          goto 44
        elseif(WORD(1:5).eq.'*form')then
          CALL EGETWR(OUTSTR,K,xdum,0.0,0.0,'-','*form',IER)
          IF(IER.NE.0)goto 1
          goto 44
        elseif(WORD(1:5).eq.'*menu')then
          CALL EGETRM(OUTSTR,K,TMENU(I),'W','*menu',IER)
          IF(IER.NE.0)goto 1
          goto 44
        elseif(WORD(1:5).eq.'*aide')then
          CALL EGETRM(OUTSTR,K,TAIDE(I),'W','*aide',IER)
          IF(IER.NE.0)goto 1
          goto 44
        elseif(WORD(1:6).eq.'*other')then
          CALL EGETWI(OUTSTR,K,NTSTAT(I),0,0,'-','NTSTAT',IER)
          CALL EGETWI(OUTSTR,K,NTBITS(I),0,0,'-','NTBITS',IER)
          itnuwpr = itnuwpr + NTBITS(I)
          goto 44

C For each item read its attributes.
        elseif(WORD(1:7).eq.'*fields')then
          CALL EGETWI(OUTSTR,K,ifield,0,0,'-','ifield',IER)
          if(ifield.ne.(NTSTAT(I)+NTBITS(I)))then
            write(outs,*) 'mismatched ifield and ntstat ntbits ',
     &        ifield,NTSTAT(I),NTBITS(I)
            call edisp(iuout,outs)
          endif

C For each of the fields build up text strings which will hold the data.
C This is done so that the ordering of the fields and their variant 
C record structures can be accommodated.
          NTMAR(I)=0
          NTMAT(I)=0      
          NTMAI(I)=0      
          DO 60 J=1,IFIELD
            CALL LSTRIPC(iafil,outstr,0,ND,1,'field data',IER)
            K=0
            CALL EGETW(OUTSTR,K,WORD,'W','field tags',IFLAG)
            if(WORD(1:4).eq.'REAL')then
              K=0
              CALL EGETW(OUTSTR,K,TMP,'W','REAL',IFLAG)
              CALL EGETWI(OUTSTR,K,IA,0,0,'-','IA',IER)
              CALL EGETWI(OUTSTR,K,IC,0,0,'-','IC',IER)
              CALL EGETWR(OUTSTR,K,TMAR(I,IA),0.0,0.0,'-','TMAR()',IER)
              CALL EGETWR(OUTSTR,K,TMIN(I,IA),0.0,0.0,'-','TMIN()',IER)
              CALL EGETWR(OUTSTR,K,TMAX(I,IA),0.0,0.0,'-','TMAX()',IER)
              CALL EGETRM(OUTSTR,K,TDFDES(I,J),'W','TDFDES(I,J)',IER)
              IATDF(I,J)=IA
              ITCOL(I,J)=IC
              ITDFTR(I,J)=1
              NTMAR(I)=NTMAR(I)+1
            elseif(WORD(1:4).eq.'TEXT')then
              K=0
              CALL EGETW(OUTSTR,K,WORD,'W','TEXT',IFLAG)
              CALL EGETWI(OUTSTR,K,IA,0,0,'-','IA',IER)
              CALL EGETWI(OUTSTR,K,IC,0,0,'-','IC',IER)
              CALL EGETW(OUTSTR,K,TMAT(I,IA),'W','TMAT(I,IA)',IFLAG)
              CALL EGETRM(OUTSTR,K,TDFDES(I,J),'W','TDFDES(I,J)',IER)
              IATDF(I,J)=IA
              ITCOL(I,J)=IC
              ITDFTR(I,J)=2
              NTMAT(I)=NTMAT(I)+1
            elseif(WORD(1:4).eq.'INTG')then
              K=0
              CALL EGETW(OUTSTR,K,WORD,'W','INTG',IFLAG)
              CALL EGETWI(OUTSTR,K,IA,0,0,'-','IA',IER)
              CALL EGETWI(OUTSTR,K,IC,0,0,'-','IC',IER)
              CALL EGETWI(OUTSTR,K,ITMAR(I,IA),0,0,'-','ITMAR()',IER)
              CALL EGETWI(OUTSTR,K,ITMIN(I,IA),0,0,'-','ITMIN()',IER)
              CALL EGETWI(OUTSTR,K,ITMAX(I,IA),0,0,'-','ITMAX()',IER)
              CALL EGETRM(OUTSTR,K,TDFDES(I,J),'W','TDFDES(I,J)',IER)
              IATDF(I,J)=IA
              ITCOL(I,J)=IC
              ITDFTR(I,J)=3
              NTMAI(I)=NTMAT(I)+1
            endif
  60      CONTINUE
          goto 44
        elseif(WORD(1:6).eq.'*end_item')then
          goto 34
        endif
      elseif(WORD(1:9).eq.'*end_item')then
        goto 34
      elseif(WORD(1:9).eq.'*pointers')then

C If just checking return to calling routine.
        if(act(1:1).eq.'c') return

C Check that NUWPR is the same as that found from adding up
C ntbits() for each item. If it differs, warn user and reset.
       if(nuwpr.ne.itnuwpr)then
         write(outs,'(a,i2,a,i2)') 'Found different number of columns',
     &     nuwpr,' from actual requirements ',itnuwpr
         call edisp(iuout,outs)
         write(outs,'(a,i2,a)') 'Shifting to ',itnuwpr,' columns.'
         call edisp(iuout,outs)
         nuwpr=itnuwpr
       endif

C We have sufficient information to create a new scratch file
C so jump to 145 to do this (code in common with the 'i' version.
        goto 145
      elseif(WORD(1:13).eq.'*tabular_data')then

C Clear the maximum and minimum for data columns. For colums which
C are active set inital ranges to trap minimums about zero and 
C maximums below zero.
        do 48 IJ=1,nwpr
          if(IJ.le.NITDF)then
            TABUMN(IJ)=+1.E+7
            TABUMX(IJ)=-1.E+7
          else
            TABUMN(IJ)=0.0
            TABUMX(IJ)=0.0
          endif
  48    continue

C Note tabular data might be up to 1k characters wide on 
C each line and might extend to additional lines.
        DO 46 IP=1,NDBSTP
          CALL CLRTAB
          CALL STRIPC1K(iafil,loutstr,0,ND,1,'tabular data',IER)
          K=0
          CALL EGETWR(LOUTSTR,K,TIMEJD,0.,0.,'-','TIMEJD',IER)

C For nuwpr items read as many lines as required.
          IRVA=nuwpr
          call EGETAGWRA(loutstr,K,iafil,IRVA,TABU,0.00,1.00,'-',
     &        'raw tabular data',IER)
          if(itrc.gt.1)then
            write(outs,*) 'at time ',TIMEJD,'tabu is ',TABU
            call edisp(iuout,outs)
          endif
          do 47 j=1,nwpr
            if(TABU(J).lt.TABUMN(J))TABUMN(J)=TABU(J)
            if(TABU(J).gt.TABUMX(J))TABUMX(J)=TABU(J)
  47      continue

C Calculate the julian day fraction RDOTY and check to see if the time
C that has been read TIMEJD is within 0.0006 day tollerance in the case
C of a >10 minute timestep and 0.00034 day for a one minute timestep.
          CDAY=AINT(FLOAT(IP)/FLOAT(NTSPH*24))
          ITIME=IP-(INT(CDAY)*NTSPH*24)
          RDOTY=REAL(itdbdoy)+CDAY+(REAL(ITIME)/(REAL(NTSPH)*24.))
          toler=(1./(24.0*float(NTSPH)))*0.5
          if(NTSPH.le.6)then
            call eclose(RDOTY,TIMEJD,0.0006,close)
          else
            call eclose(RDOTY,TIMEJD,0.00034,close)
          endif
          if(.NOT.close)then
            write(iuout,*)'RDOTY ne TIMEJD @',ip,RDOTY,TIMEJD,toler
          else

C The records for timstep data begins at 50.
            ITREC=IP+49
          endif
          CALL EWTABU(ITRC,ITREC,IER)
   46   CONTINUE

C Now write the maximum for each column and then the last file record.
        IREC=48
        WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103) (TABUMX(J),J=1,NWPR)
        IREC=49
        WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103) (TABUMN(J),J=1,NWPR)
        IREC=50+NDBSTP
        tail='ENDTDF  '
        WRITE(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103) tail

      elseif(WORD(1:17).eq.'*end_tabular_data')then
         CALL ERPFREE(iafil,ISTAT)
         call edisp(iuout,'Importing data finished.')

C Debug.
C         write(6,*)'tabumn ',TABUMN
C         write(6,*)'tabumx ',TABUMX

         return
      else
        goto 34
      endif
      goto 34

  99  CALL ERPFREE(iafil,ISTAT)
      RETURN

C Otherwise we have sufficient information to create a new scratch file.
C Create it with minimal width if possible given the actually used
c records within the ASCII version.
 145  continue
      OPTDF=.FALSE.
      CALL ERPFREE(IUTDF,ISTAT)
      XST=.false.
      call findfil(LTDF,XST)
      if(XST)then

C Delete the existing file (this requires opening it and then
C calling efdelet. Then open new scratch file with the minimal
C width.
        WRITE(OUTS,'(A,A)')' Deleting existing ',LTDF(1:lnblnk(LTDF))       
        CALL EDISP(IUOUT,OUTS)
        ier=0
        call EFOPRAN(iutdf,LTDF,ITWPR,1,IER)
        CALL EFDELET(iutdf,ISTAT)
      endif

C Use logic for minimal width opening of the scratch file.
C Longest item its attributes take up 3+2+8+9+1+1 words (24).

C Debug.
C      write(6,*) 'a nwpr=',nwpr,' nuwpr=',nuwpr,' itwpr=',itwpr,
C     &  ' ndata',ndata

      if(nuwpr.eq.1)then
        ITWPR= 24
        NWPR= ITWPR-1
      elseif(nuwpr.gt.1.and.nuwpr.lt.24)then
        ITWPR= 24
        NWPR= ITWPR-1
      elseif(nuwpr.ge.24)then
        ITWPR=NUWPR+1
        NWPR= ITWPR-1
      endif

C Debug.
C      write(6,*) 'b nwpr=',nwpr,' nuwpr=',nuwpr,' itwpr=',itwpr,
C     &  ' ndata',ndata

      ier=0
      call EFOPRAN(iutdf,LTDF,ITWPR,3,IER)
      WRITE(OUTS,'(A,A)')' Opened new ',LTDF(1:lnblnk(LTDF))       
      CALL EDISP(IUOUT,OUTS)
      OPTDF=.TRUE.
        
C Update the scratch file. If ndata has been lost, reset from nitdf.
      if(ndata.eq.0) ndata=nitdf
      IRVA=NDATA
      CALL EGETWIA(iafil,IHREC,IRVA,0,0,'-','pointers',IER)
      call usrmsg(' ','Updating the scratch file...','-')
      CALL EWTDF(IER)
      call usrmsg(' ','Updating the scratch file...done.','-')
      goto 34

C Export file write errors.
 1    CALL LUSRMSG('Error in importing data',OUTSTR,'W')
      IER=1
      goto 99
 103  CALL LUSRMSG('Error in writing importing data',OUTSTR,'W')
      IER=1
      goto 99

      END

C ************* ERTABU 
C ERTABU reads one timesteps info from binary TDF file. 
C ITREC is the timestep record to read. ITRC is the 
C trace level, IER=0 OK, IER=1 problem. 
      SUBROUTINE ERTABU(ITRC,ITREC,IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN

C Single timestep of TABULAR data and temporary storage TABT.
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

      CHARACTER OUTS*160

C Read tabular data for the given timestep and for the given
C instance into TAB. Convert from timestep into representation of time.
C Figure out a way not to print this heading when not needed.
      IF(ITRC.GT.1.AND.ITREC.EQ.50)THEN
        CALL EDISP(IUOUT,' Reading tabular timestep data...')
        CALL EDISP(IUOUT,
     &  '  T/S  Rec Col 1 Col 2 Col 3 Col 4 Col 5 Col 6 Col 7 Col 8..')
      ENDIF
      ITS=ITREC-49
      CALL CLRTAB
      READ(IUTDF,REC=ITREC,IOSTAT=ISTAT,ERR=103) (TABU(J),J=1,NWPR)

      IF(ITRC.GT.1)THEN
        nss=min0(27,NWPR)
        WRITE(iuout,'(A,2I5,27F6.1)')'< ',ITS,ITREC,(TABU(J),J=1,nss)
      ENDIF

  99  RETURN

C TABU read errors.
 103  write(outs,'(A,I6,A,I6)')' @ record ',ITREC,' & timestep ',ITS
      CALL USRMSG(' Problem reading tabular data ',outs,'W')
      IER=1
      goto 99

      END

C ************* ERTABUPREV 
C ERTABUPREV reads one timesteps info from binary TDF file into TABUPREV array. 
C ITREC is the timestep record to read. ITRC is the 
C trace level, IER=0 OK, IER=1 problem. 
      SUBROUTINE ERTABUPREV(ITRC,ITREC,IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN

C Single timestep of TABULAR data and temporary storage TABUPREV.
      COMMON/TDATPREV/TABUPREV(MTABC)

      CHARACTER OUTS*160

C Read tabular data for the given timestep and for the given
C instance into TAB. Convert from timestep into representation of time.
C Figure out a way not to print this heading when not needed.
      IF(ITRC.GT.1.AND.ITREC.EQ.50)THEN
        CALL EDISP(IUOUT,' Reading tabular timestep data...')
        CALL EDISP(IUOUT,
     &  '  T/S  Rec Col 1 Col 2 Col 3 Col 4 Col 5 Col 6 Col 7 Col 8..')
      ENDIF
      ITS=ITREC-49
      CALL CLRTABPREV
      READ(IUTDF,REC=ITREC,IOSTAT=ISTAT,ERR=103) (TABUPREV(J),J=1,NWPR)

      IF(ITRC.GT.1)THEN
        nss=min0(39,NWPR)
        WRITE(iuout,'(A,2I5,40F6.1)')'< ',ITS,ITREC,
     &    (TABUPREV(J),J=1,nss)
      ENDIF

  99  RETURN

C TABU read errors.
 103  write(outs,'(A,I6,A,I6)')' @ record ',ITREC,' & timestep ',ITS
      CALL USRMSG('ERTABUPREV: Problem reading tabular data ',outs,'W')
      IER=1
      goto 99

      END

C ************* EWTABU 
C EWTABU writes one timesteps Tabular info to TDF file from the array TAB. 
C ITREC is the record to be written.  ITRC is the trace level.
      SUBROUTINE EWTABU(ITRC,ITREC,IER)
#include "building.h"
#include "model.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      CHARACTER msgl2*48

C Write tabular data for the given timestep.
      IF(ITRC.GT.1.AND.ITREC.EQ.1)THEN
        CALL EDISP(IUOUT,' Write tabular timestep data to file:')
        CALL EDISP(IUOUT,
     &  '  Time Col 1 Col 2 Col 3 Col 4 Col 5 Col 6 Col 7 Col 8..')
      ENDIF
      ITS=ITREC-49
      WRITE(IUTDF,REC=ITREC,IOSTAT=ISTAT,ERR=103) (TABU(J),J=1,NWPR)
      IF(ITRC.GT.1)THEN
        nss=min0(23,NWPR)
        WRITE(iuout,'(A,2I5,27F6.1)')'> ',its,ITREC,(TABU(J),J=1,nss)
      ENDIF

  99  RETURN

C Tabular write errors.
 103  msgl2='  '
      CALL USRMSG(' could not write tabular data ',msgl2,'W')
      IER=1
      goto 99

      END

C ************* EWTABA 
C EWTABA exports one timesteps Tabular info. 
C ITREC is the record to be written.
C Note: only writes out the number of columns of data that are
C actually used in the file. However, if ewtaba is being called
C to alter the number of columns check the value of iadcols.
      SUBROUTINE EWTABA(ITREC,RDOTY,iafil,IER)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/TDFADDC/iadcols,deftabu(9)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      CHARACTER delim*1,msgl2*48,louts*1000,outs*124

C If nuwpr is zero then we have a problem.
      if(nuwpr+iadcols.eq.0)then
        write(outs,*) 'no colums to write at rec ',itrec,rdoty
        call edisp(iuout,outs)
        ier=1
        return
      endif

C Export tabular data for the given timestep and for the given
C instance into TAB.
      delim='C'
      IF(ITREC.EQ.50)THEN
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=2) '*tabular_data'
        WRITE(iafil,'(a)',IOSTAT=ios,ERR=2)
     &  '# Time Col 1 Col 2 Col 3 Col 4 Col 5 Col 6 Col 7 Col 8...'
      ENDIF

C Write a potentially long list. The first line will begin with
C the RDOTY (timestamp) and NUWPR+iadcols will be written after that on
C as many lines as are required.
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call arlist(ipos,nuwpr+iadcols,TABU,MTABC,delim,louts,loutln,
     &    itrunc)
        if(ipos.eq.1) then
          WRITE(iafil,'(F10.6,2a)',IOSTAT=ios,ERR=2) RDOTY,',',
     &      louts(1:loutln)
        else
          WRITE(iafil,'(1x,a)',IOSTAT=ios,ERR=2) louts(1:loutln)
        endif
        ipos=itrunc+1
      end do
  99  RETURN

C TAB file write errors.
  2   msgl2='  '
      CALL USRMSG(' could not write tabular data ',msgl2,'W')
      IER=1
      goto 99

      END

C ************* CLRTAB 
C CLRTAB clears the current working tabular timestep data.
      SUBROUTINE CLRTAB
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

C For current timestep item clear tabular array.
      DO 10 I=1,MTABC
        TABU(I)=0.0
  10  CONTINUE
      RETURN
      END


C ************* CLRTABPREV 
C CLRTABPREV clears the previous records working tabular timestep data.
      SUBROUTINE CLRTABPREV
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"
      COMMON/TDATPREV/TABUPREV(MTABC)

C For current timestep item clear tabular array.
      DO 10 I=1,MTABC
        TABUPREV(I)=0.0
  10  CONTINUE
      RETURN
      END

