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

C FILES.F of ESRUshd contains the following filing routines:

C SIFILE: Opens a shading/insolation file.
C SWRT:   Writes the shading data to the shading file.
C SSAVE:  Checks and writes a SHADING/INSOLATION FILE.
C RETRV1: Retrieves information from the header block of the
C         transitional file.
C ISAVE:  Checks and writes a SHADING/INSOLATION file
C         as required by ESPbps from insolation calculation.

C Format of shading insolation file:
C Minimum width is 24 words, normal width is is nzsur(icomp) words.
C Record 1: first 12 words holds ishd (the analysis held for each month)
C    followed by the record in the file where the data for each month begins.
C Record 2: first word is irecx (last record used), 2nd is nsurs (number of
C    surfaces in zone at the time the file was created - used to check against
C    the current number of surfaces in the zone, 3rd item is msurs (number of
C    insolated surfaces parameter at time the file was created - used to check
C    against current parameter.
C Record 3: not used, for each surface, currently filled with zeros.
C  << but might be used to hold global igc(), test for this by scanning
C  << for non-zero entries.
C Record 4: for each surface ntmc()=1 if an insolation source.
C Record 5: beginning of shading information.
C Shading records:
C   ISADD() points to the start record for shading data for each
C   surface in the zone (including surfaces not included in the
C   shading analysis). Shading data PSO() is written to nsur records
C   with 24 real numbers in each record.
C Insolation records:
C   If there is shading this starts ISADD(MON)+NSUR records later otherwise
C   it starts at ISADD(MON).
C   First record of insolation data is the IGC hash array of offsets.
C   See comment in IWRT for how the array of offsets is calculated. Beginning
C   with the next record there are MISUR records for each insolation
C   source for the insst() array, MISUR records for the pinsst() array
C   and MISUR records for the pinwst() array. Note: pinwst is always
C   a zero filled array.

C ************** SIFILE 
C SIFILE: Opens a shading/insolation file, if it exists then it scans
C the file and fills common block data (DATA1, shdglob, stins).
      SUBROUTINE SIFILE(ICOMP)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
C insrtd is toggle if zero then shading data has not yet been inserted 
C   and is set to one after data is written. It is reset to zero in
C   subroutine comput when the user changes shading directives.
C instri is toggle if zero then header has not yet been updated after
C   a new months insolation data has been updated/appended. It is reset
C   to zero in inscom when user changes insolation directives.
      COMMON/SIF1/INSRTD,INSTRI
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/ract/paction
      LOGICAL XST
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL,FNAME
      character outs*124,paction*16
      
C ISADD array is the record associated with the start of data for
C each month.
C ISHD is the type of analysis (shading or insolation or shading & insolation)
C for each month. 
C << these are read and written many times - consider as a common block >>
      DIMENSION ISADD(12),ISHD(12)

C Close any file already open then assign a new file.
      IFILSI=IFIL+3
      CALL ERPFREE(IFILSI,ISTST)
      if(ISI(ICOMP).eq.1)then
        FNAME=LSHAD(ICOMP)
      else
        call usrmsg(' No shading:insolation file name found in the',
     &              ' model configuration. Please define this.','W')
        return
      endif
      maxrec=24
      if(NZSUR(icomp).gt.24)maxrec=NZSUR(icomp)
      call FINDFIL(FNAME,XST)
      IF(XST)THEN
        call EFOPRAN(IFILSI,FNAME,maxrec,1,IER)
        IF(IER.ne.0)then
          CALL edisp(IUOUT,' Error opening the file (maxrec words)...')
          return
        endif

C Check file has been previously written to. If a problem the try again
C at maxrec=24 for then MS words wide.
        IREC=2
        READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1000)INEXT,NS
        if(INEXT.LE.3.OR.NS.NE.NZSUR(icomp))then
          CALL ERPFREE(IFILSI,ISTAT)
          maxrec=24
          call EFOPRAN(IFILSI,FNAME,maxrec,1,IER)
          IF(IER.ne.0)then
            CALL edisp(IUOUT,' Error opening file (24 words)...')
            return
          endif
          READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1000)INEXT,NS
          if(INEXT.LE.3.OR.NS.NE.NZSUR(icomp))then
            CALL ERPFREE(IFILSI,ISTAT)
            maxrec=MS
            call EFOPRAN(IFILSI,FNAME,maxrec,1,IER)
            IF(IER.ne.0)then
              CALL edisp(IUOUT,' Error opening file (MS words)...')
              return
            endif
            READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1000)INEXT,NS
            if(INEXT.LE.3.OR.NS.NE.NZSUR(icomp))goto 1003
          endif
        endif
      ELSE

C If new file fill first record with 0 so will be interpreted as empty.
        call EFOPRAN(IFILSI,FNAME,maxrec,3,IER)
        IREC=1
        do 15 I=1,12
          ISADD(I)=0
          ISHD(I)=0
  15    continue
        WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(ISHD(I),I=1,12),
     &       (ISADD(I),I=1,12)
        IF(ISTAT.lt.0)then
          CALL edisp(IUOUT,' Sorry - error writing ISHD & ISADD')
          GOTO 1100
        endif
      ENDIF

C Return after advising user and setting insrtd & instri to zero
C to indicate to subroutines comput and inscom that no data has
C been saved.
      if(paction(1:13).ne.'update_silent')then
        write(outs,'(A,A)')' Opened Shad:Insol db ',FNAME
        call edisp(iuout,outs)
        call usrmsg(' Scanning shading:insolation file...done.','  ',
     &    'P')
      endif
      INSRTD=0
      INSTRI=0
  900 RETURN

 1100 call edisp(iuout,'ish: file initial read error in `SIFILE` ')
      goto 900
 1000 call edisp(iuout,'ish: file initial scan error in `SIFILE` ')
      goto 900
 1003 write(outs,'(a,i2,a,i2,a,i2)') 'Model nsur =',NZSUR(icomp),
     &  ' file ns =',ns,' inext=',INEXT
      call edisp(iuout,' Mismatch between model and file.')
      call edisp(iuout,outs)
      goto 900
      END

C ************** SWRT **********************
C SWRT: Writes the shading data to the shading file.
C IREC is the start record where data for the months shading data
C is written. IREC is updated to the next record after the
C insolation hash array position when the subroutine returns.
      SUBROUTINE SWRT(IREC)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/DATA1/PSO(MS,24)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      DIMENSION IGC(MS)
      character outs*124

      IFILSI=IFIL+3
      
C Beginning at IREC write out 24 hours of shading data for each surface
C in the zone (even for surfaces not included in the analysis). Reset
C insolation hash igc() to zero.
      DO 100 I=1,NSUR
        WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1000)(PSO(I,J),J=1,24)
        IREC=IREC+1
        IGC(I)=0
  100 CONTINUE

C At end of surfaces, write zeroed insolation hash array (shading is
C calculated prior to insolation so recalculating shading
c will most likely require a recalculation of insolation).
      IGCR=IREC
      WRITE(IFILSI,REC=IGCR,IOSTAT=ISTAT,ERR=1000)(IGC(I),I=1,NSUR)
      
C Increment irec before returning.
      IREC=IREC+1
  140 RETURN

C Error handling
 1000 WRITE(outs,1009)IREC
 1009 FORMAT(' Shading/Insolation file error in `SWRT` - record',I6)
      call edisp(iuout,outs)
      goto 140
      END

C ************** SSAVE *********************
C SSAVE: Checks and writes shading portion of a SHADING/INSOLATION FILE.
C Called from subroutine comput once per month.
C It returns ier as non-zero if there is a problem.
      SUBROUTINE SSAVE(ier)
#include "building.h"

      COMMON/DATA2/ICAL
      common/ract/paction
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C isc()=1 surface included in shading analysis isc()=0 not included.
C       isc is saved in transitional file and assumes that different
C       list of surfaces might be included each month.
C       << possible revision to have isc global (i.e. apply to all months) >>
C isadd() record at start of each month.
C ishd()=0 nothing for this month, ishd()=1 shading only, ishd()=2 insolation only,
C       ishd()=3 shad & insul.
C ntmc()=1 surface is source of insulation, ntmc()=0 not an insulation source.
C igc() is an offset to the insolation data.
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/FILEP/IFIL
      COMMON/SIF1/INSRTD,INSTRI

      DIMENSION ISADD(12),ISHD(12),PPS(24),IGC(MS),ntmc(ms)
      dimension NGLDUM(MS)
      character outs*124,paction*16,h*72
      logical OK,dok

      IFILSI=IFIL+3
      ier=0

C Clear local arrays.
      do 15 I=1,12
        ISADD(I)=0
        ISHD(I)=0
  15  continue
      do 16 I=1,MS
        IGC(I)=0
        ntmc(I)=0
        NGLDUM(I)=0
  16  continue
      do 17 I=1,24
        PPS(I)=0.
  17  continue

C Check SHADING/INSOLATION file, if at least one item of ishd and isadd array
C is none zero then there is data in the file.
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=30)(ishd(i),i=1,12),
     &(isadd(i),i=1,12)
      do 10 i=1,12
        if(ishd(i).ne.0.or.isadd(i).ne.0)goto 20
   10 continue
      goto 30

C File contains data. Read header information.
C irecx is the last record used in the file.
   20 irec=2
      read(ifilsi,rec=irec,iostat=istat,err=1100)irecx,nsurs,msurs
      if(nsur.ne.nsurs.or.msurs.ne.misur)then
        call edisp(iuout,' Surfaces don`t match: nsur<>nsurs')
        goto 1000
      endif

C Read which surfaces are sources of insolation (yes if ntmc(i)=1).
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=1100)(ntmc(i),i=1,nsur)
      goto  40

C File doesn't exist: create one.
   30 do 31 i=1,12
        ishd(i)=0
        isadd(i)=0
   31 continue
      do 32 i=1,nsur
        ntmc(i)=0
   32 continue
      irecx=5
      if(paction(1:13).ne.'update_silent')then
        call edisp(iuout,' Creating new shading/insolation file ')
      endif

C The file exists (or has been created). Check if any shading data is
C to be added.
   40 IF(ICAL.NE.0)goto 310
      call edisp(iuout,' No shading data to be saved ')
      goto 900

C Check that data has not been saved already.
  310 IF(INSRTD.EQ.0)goto 320
      call edisp(iuout,' This data has already been saved')
      goto 900

C Inserting data, so ensure file is type 1 or 3 (upgrade from no
C data to shading only or from insolation only to shading and insolation).
  320 ISHDT=ISHD(MON)
      IF(ISHDT.EQ.0)ISHD(MON)=1
      IF(ISHDT.EQ.2)ISHD(MON)=3

C Check if data to be inserted clashes with data already in the file.
C Recover the last record used (from irecx). If only insolation was 
C calculated for the current month recover the record where that data
C was stored (insolr).
      IREC=IRECX
      IF(ISHDT.EQ.2)INSOLR=ISADD(MON)
      
C If no shading data exists for the current month jump to insertion
C code (using the last available record position).
      IF(ISHDT.NE.1.AND.ISHDT.NE.3)goto 500

C Shading data already exists in this position
      dok=.true.
      h(1)='This month had been previously calculated. Probably you'
      h(2)='will want to overwrite this with the new calculations. '
      h(3)=' '
      h(4)='And it is safer to have removed the zone shading files '
      h(5)='and write fresh files rather than overwriting old files. '
      CALL ASKOK(' Data for this month already exists. The new data',
     &' to be saved will overwrite this. Is this OK?',OK,dok,5)
      IF(.NOT.OK)goto 900

C Check if new data will fit into the existing data space in the file.
C Read the insolation hash array (for this month), increment the record
C number and remember in INSOLR.
      IREC=ISADD(MON)+NSUR
      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(IGC(I),I=1,NSUR)
      IREC=IREC+1
      NR=0
C << NOTE cannot find any code that would set IGC to < zero >>
      DO 410 I=1,NSUR
        if(IGC(I).LT.0)then
          call edisp(iuout,' Checking for space: IGC < 0')
          goto 1000
        endif
  410 CONTINUE
      INSOLR=IREC
      
C Jump back to start record for this month and read first item into
C the first IGC position. If this read sucessful then jump to data
C insertion code, if it cannot be done 
      IREC=ISADD(MON)
      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=450)IGC(1)
      goto 500

C DATA IS LAST DATA IN FILE, SO OVERWRITE.
  450 IREC=ISADD(MON)

C Insert data, note that IREC is one record past the shading data
C when swrt returns.
  500 ISADD(MON)=IREC
      CALL SWRT(IREC)

C Check if any insolation data to be kept. Read from current location
C (insolr) and write to new position (irec).
      IF(ISHD(MON).NE.3)goto 600
      IF(IREC.EQ.INSOLR)goto 600
      READ(IFILSI,REC=INSOLR,IOSTAT=ISTAT,ERR=1100)(IGC(I),I=1,NSUR)
      INSOLR=INSOLR+1
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(IGC(I),I=1,NSUR)
      IREC=IREC+1
      DO 520 I=1,NSUR
        IF(IGC(I).EQ.0)goto 520
        
C The number of records associated this this insolation source is 3*MISUR.
C Lopp through each of these records reading data and writing to new location.
        NWINREC=ntmc(i)*misur*3
        DO 510 J=1,NWINREC
          READ(IFILSI,REC=INSOLR,IOSTAT=ISTAT,ERR=1100)(PPS(K),K=1,24)
          INSOLR=INSOLR+1
          WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(PPS(K),K=1,24)
          IREC=IREC+1
  510   CONTINUE
  520 CONTINUE

C Write header. If past earlier irecx update last used record.
  600 IF(IREC.GT.IRECX)IRECX=IREC
      IREC=1
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(ISHD(I),I=1,12),
     &(ISADD(I),I=1,12)
      IREC=2
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)IRECX,NSUR,misur
      irec=3

C With no default windows NGL is always zero.
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(NGLDUM(I),I=1,NSUR)
      irec=4
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(ntmc(i),i=1,nsur)
      INSRTD=1

C Return to menu.
  900 CONTINUE
      RETURN

C Error handling.
 1000 call edisp(iuout,' Geometry used in the SHADING/INSOLATION db')
      call edisp(iuout,' DOES NOT Correspond to the geometry file you')
      call edisp(iuout,' have specified. Try another database.')
      ier=1
      goto 900

 1100 WRITE(outs,1109)IREC
 1109 FORMAT(' Shading/Insolation file error in SSAVE - record',I6)
      call edisp(iuout,outs)
      ier=1
      goto 900
      END

C ************** RETRV1 *********************
C 'RETRV1' Retrives all input information from the header block of the
C transitional shading file for zone icomp)
      SUBROUTINE RETRV1(icomp)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      character ETYPE*4
      real gversion
      integer igupgrade
      COMMON/G0/ETYPE(MCOM),gversion(MCOM),igupgrade
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/GS6/NOX,NOZ,NGX,NGZ,BLKNAME(MB),BLKMAT(MB)
      COMMON/GS5/NB,XO(MB),YO(MB),ZO(MB),DX(MB),DY(MB),DZ(MB),ANG(MB)
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/HEADL/IRECS(MS)
      CHARACTER BLKNAME*8,BLKMAT*12,outs*124

      IUNIT=IFIL
      
C The initial records holds a mix of the G1 common block for the
C associated zone along with:
C  MON - the month (mon) index associated with this transitional file.     
C  ISC() - array indicating surfaces included in the shading analysis.
C  IYD - the julian day of the analysis.
C << alternative is to recover G1 commons via reading the geometry file >>
C << alternative is to have a global ISC array (same for all months) >>
      IREC=3
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ETYPE(icomp),NSUR,NTV,
     & (NVER(I),I=1,NSUR)
      IREC=2
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)MON,(ISC(I),I=1,NSUR),
     & IYD
      IREC=IREC+2
      DO 10 I=1,NSUR
        NV=NVER(I)
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(JVN(I,J),J=1,NV)
        IREC=IREC+1
   10 CONTINUE
   
C Read in XYZ coordinates of each vertex depending on the number of
C vertices in the zone. Read 3 records of data if less than 20
C vertices, 6 records if less than 40 etc.
      IF(NTV.GT.20)goto 1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,NTV)
      IREC=IREC+1
      goto 2
    1 IF(NTV.GT.40)goto 3
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,NTV)
      IREC=IREC+1
      goto 2
    3 IF(NTV.GT.60)goto 44
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=41,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=41,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=41,NTV)
      IREC=IREC+1
      goto 2
   44 IF(NTV.GT.80)goto 55
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=61,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=61,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=61,NTV)
      IREC=IREC+1
      goto 2
  55  IF(NTV.GT.100)goto 56
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=81,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=81,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=81,NTV)
      IREC=IREC+1
      goto 2
  56  IF(NTV.GT.120)goto 57
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=101,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=101,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=101,NTV)
      IREC=IREC+1
      goto 2

  57  IF(NTV.GT.140)goto 58
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=121,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=121,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=121,NTV)
      IREC=IREC+1
      goto 2

  58  IF(NTV.GT.160)goto 59
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=121,140)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=141,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=121,140)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=141,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=121,140)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=141,NTV)
      IREC=IREC+1
      goto 2

  59  continue
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=121,140)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=141,160)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(X(I),I=161,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=121,140)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=141,160)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Y(I),I=161,NTV)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=1,20)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=21,40)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=41,60)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=61,80)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=81,100)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=101,120)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=121,140)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=141,160)
      IREC=IREC+1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Z(I),I=161,NTV)
      IREC=IREC+1

    2 continue
      IREC=IREC+1

C Skip to obstruction blocks and fill GS5 and GS6 commons.
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NB
      IREC=IREC+1
      DO 40 I=1,NB
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)XO(I),YO(I),ZO(I),
     &  DX(I),DY(I),DZ(I),ANG(I)
        IREC=IREC+1
   40 CONTINUE
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NOX,NOZ,NGX,NGZ
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IRECS(I),I=1,NSUR)
    5 RETURN

 1000 write(outs,4)IREC
    4 FORMAT(' Transitional shading file error in RETRV1 - rec',I6)
      call edisp(iuout,outs)
      goto 5
      END

C ********** IWRT
C This routine writes the insolation data to the shading/insolation
C file. IREC is the initial record for insolation data and holds the
C offsets needed to locate data for each insolation source. IREC is
C returned as the next record after the insolation data that has
C been written.
      SUBROUTINE IWRT(IREC)

#include "building.h"
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/OUTIN/IUOUT,IUIN
      common/filep/ifil
      
C For each surface, itmc()=1 indicates an insolation source.
C nwins is the number of insolation sources in zone.
C itmcfg is one if there is at least one insolation source.
      common/tmc/itmc(ms),itmcfg,nwins
      common/stins/insst(mgt,24,misur),pinsst(mgt,24,misur),
     &             pinwst(mgt,24,misur),shadst(mgt,24)

      DIMENSION IGC(MS)
      character outs*124

      ifilsi=ifil+3

C IGC is the record offset from the end of the shading data to the
C insolation data for a particular surface. (IGC(i) is non-zero if
C it is a 'source' surface.
C To calculate position values for IGC - loop through each surface in the
C zone, if shading has not been calculated for the surface (isc()=0)
C then skip, otherwise set IGC to last value of NW and then increment
C NW. IGC(1) is always one. 
      NW=1
      DO 50 I=1,NSUR
        IGC(I)=0
        if(ISC(I).ne.0)then

C Increment NW by the maximum number of insolated surfaces (misur)
C if surface i is a 'source'.
          IGC(I)=NW
          NW=NW+itmc(i)*misur
        endif
   50 CONTINUE

      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(IGC(I),I=1,NSUR)
      IREC=IREC+1

C Output data, loop through each of the 'sources' and write out hourly insst
C toggle for each of the surfaces which can receive insolation.
      do 200 i=1,nwins
        do 201 j=1,misur
        WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(insst(i,n,j),n=1,24)
          IREC=IREC+1
  201   continue

C Write out hourly pinsst for each of the surfaces which can receive insolation.
        do 202 j=1,misur
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinsst(i,n,j),n=1,24)
          IREC=IREC+1
  202   continue

C Write out hourly pinwst (will always be 0.0) for each of the surfaces
C which can receive insolation.
C << work to remove this set of records >>
        do 203 j=1,misur
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=900)(pinwst(i,n,j),n=1,24)
          IREC=IREC+1
  203   continue
  200 CONTINUE

    1 RETURN

C Error handling.
  900 WRITE(outs,1009)IREC
 1009 FORMAT(' Shading/Insolation file error in IWRT - record',I6)
      call edisp(iuout,outs)
      goto 1
      END


C *************** ISAVE
C ISAVE checks and writes insolation data to a SHADING/INSOLATION file.
C It is called by inscom.
      SUBROUTINE ISAVE

#include "building.h"
      COMMON/DATA2/ICAL
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/CONTR/MON,ISC(MS),IYD
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/filep/ifil

      common/tmc/itmc(ms),itmcfg,nwins
      COMMON/SIF1/INSRTD,INSTRI
      common/ract/paction
      character outs*124,paction*16,h*72
      logical OK,dok

      DIMENSION ISADD(12),DUM(24),ISHD(12),IGC(MS),ntmc(ms)
      DIMENSION NGLDUM(MS)
      
C Assuming unit ifil+3.
      ifilsi=ifil+3

C Clear arrays associated with the shading/insolation file header
C before scanning the header.
      do 15 I=1,12
        ISADD(I)=0
        ISHD(I)=0
  15  continue
      do 16 I=1,MS
        IGC(I)=0
        ntmc(I)=0
        NGLDUM(I)=0
  16  continue
      do 17 I=1,24
        DUM(I)=0.
  17  continue

C Scan the header of the file.
      irec=1
      read(ifilsi,rec=irec,iostat=istat,err=30)(ishd(i),i=1,12),
     &    (isadd(i),i=1,12)
      do 10 i=1,12
        if(ishd(i).ne.0.or.isadd(i).ne.0)goto 20
   10 continue
   
C If we got to here then there was no data in the file so jump
C to creation code.
      goto 30

C File contains data. Read header information and check to see if
C it matches current zone definition.
   20 irec=2
      read(ifilsi,rec=irec,iostat=istat,err=1100)irecx,nsurs,msurs
      if(nsur.ne.nsurs.or.msurs.ne.misur)then
        call edisp(iuout,'Surfaces don`t match: nsur<>nsurs')
        goto 1000
      endif

C Find out which surfaces are insolation sources and if this
C these sources are `new knowledge` inform the user.
      irec=4
      read(ifilsi,rec=irec,iostat=istat,err=1100)(ntmc(i),i=1,nsur)

C Next lines commented out of SHD version.
      do 26 i=1,nsur
        if(itmc(i).ne.ntmc(i))then
          write(outs,'(A,I2)')
     &     ' Updating shd:ins db knowledge of transparent surface ',i
          if(paction(1:13).ne.'update_silent')then
            call edisp(iuout,outs)
          endif
        endif
   26 continue
      goto 40

C File doesn't exist: create one. Note ishd and isadd are still zero
C at this point. Set next available record to 5.
   30 irecx=5
      if(paction(1:13).ne.'update_silent')then
        call edisp(iuout,' Creating new shading/insolation file')
      endif

C The file exists (or has just been created).
C Check if any INSOLATION data is to be added.
   40 IF(ICAL.NE.0)goto 310
      call edisp(iuout,' No shading data to be saved ')
      GOTO 900

C Check that data has not been saved already.
  310 IF(INSTRI.EQ.0)goto 320
      call edisp(iuout,' This data has already been saved')
      GOTO 900

C Inserting data, so ensure ishd() is type 2 or 3.
  320 ishdt=ishd(mon)
      if(ishdt.eq.0)then
        ishd(mon)=2
      elseif(ishdt.eq.1)then
        ishd(mon)=3
      endif

C Check if data to be inserted clashes with data already in the file.
      irec=irecx
      if(ishdt.eq.0)then
        isadd(mon)=irecx
        goto 500
      endif

C Data already exists in this position.
      if(ishdt.eq.1)goto 400
      dok=.true.
      h(1)='This month had been previously calculated. Probably you'
      h(2)='will want to overwrite this with the new calculations. '
      h(3)=' '
      h(4)='And it is safer to have removed the zone shading files '
      h(5)='and write fresh files rather than overwriting old files. '
      CALL ASKOK('Data for this month already exists. The new data',
     &' to be saved will overwrite this. Is this OK?',OK,dok,5)
      IF(.NOT.OK)goto 900

C Check if new data will fit into the existing data space in the file.
  400 irec=isadd(mon)
      if(ishdt.eq.2)goto 430

C Skip shading data by jumping NSUR records past the start of information
C for month MON and read the igc array.
      IREC=ISADD(MON)+NSUR
      READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(IGC(I),I=1,NSUR)
      IREC=IREC+1
      DO 410 I=1,NSUR
        IF(IGC(I).LT.0)then
          call edisp(iuout,' Found IGC < 0')
          goto 1000
        endif
  410 CONTINUE

C Check size of insolation data (read the igc hash array).
  430 READ(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=500)(IGC(I),I=1,NSUR)
      IF(ishdt.EQ.1)goto 460
      NR=0
      DO 450 I=1,NSUR
      IF(IGC(I).LT.0)then
        call edisp(iuout,' Found IGC < 0')
        goto 1000
      endif
      IF(IGC(I).NE.0)NR=NR+itmc(i)
      IF(ISC(I).NE.0)NR=NR-itmc(i)
  450 CONTINUE
      IF(NR.GE.0)goto 500

C COPY SHADING DATA TO END OF FILE
  460 ISHDR=ISADD(MON)
      ISADD(MON)=IRECX
      ISHDSZ=IREC-ISHDR
      IREC=IRECX
      IF(ishdt.EQ.2)goto 490
      DO 480 I=1,ISHDSZ
      READ(IFILSI,REC=ISHDR,IOSTAT=ISTAT,ERR=1100)(DUM(J),J=1,24)
      ISHDR=ISHDR+1
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(DUM(J),J=1,24)
      IREC=IREC+1
  480 CONTINUE
  490 CONTINUE

C Insert data
  500 CALL IWRT(IREC)

C Write header, if the current record is beyond irecx then shift irecx.
      IF(IREC.GT.IRECX)IRECX=IREC
      IREC=1
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(ISHD(I),I=1,12),
     &(ISADD(I),I=1,12)
      IREC=2
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)IRECX,NSUR,misur
      irec=3
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(NGLDUM(I),I=1,NSUR)
      irec=4
      WRITE(IFILSI,REC=IREC,IOSTAT=ISTAT,ERR=1100)(itmc(i),i=1,nsur)
      INSTRI=1

C Return to menu.
  900 return

C Error handling
 1000 call edisp(iuout,' Geometry used in the SHADING/INSOLATION db')
      call edisp(iuout,' DOES NOT Correspond to the geometry file you')
      call edisp(iuout,' have specified.')
      GOTO 900

 1100 WRITE(outs,1109)IREC
 1109 FORMAT(' Shading/Insolation file error in SSAVE - record',I6)
      call edisp(iuout,outs)
      GOTO 900
      END

