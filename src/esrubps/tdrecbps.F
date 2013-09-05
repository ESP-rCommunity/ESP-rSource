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

C This file contains subroutines which allow data to be recovered from
C a TDF binary (scratch) file.

C ************* RCTDFB ************************************************
C RCTDFB recovers data from the TDF binary (scratch) file for a named instance
C at a given timestep.
C 'time' is the time (btimep or btimef) from the simulation.
C Data is put in VAL (an array sized to hold single item data fields).
C IFOC is the instance array index within the file.
C ISDAT is a flag indicating which VAL index the period data begins at. 
C IDC maps fields >> VAL index where VAL(1) is period start time
C (decimal hours), VAL(2) is end, VAL(3...) are the first period fields.
C ITCC maps fields >> VAL index where VAL(1) is the fractional Julian
C day and VAL(2...) are timestep fields.

      SUBROUTINE RCTDFB(ITRC,time,VAL,ISDAT,IFOC,IER)
#include "building.h"
#include "tdf2.h"
#include "sbem.h"
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      COMMON/TDATPREV/TABUPREV(MTABC)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/tdfrem/ialrdy_warned

C For each timestep (assuming 10 tsph frequency over a year) iknowrec
C will be zero if not yet set and a positive number representing the record
C to read if already known. 1st value is tdf record,
C  2nd value is day for this timestep,
C  3rd value is timestep in the day for this timestep,
C  4th value is whether to interpolate for this timestepX.
      integer iknowrec,itdflastrec,itdfprevrec
      common/tdfhash/iknowrec(87600,4),itdflastrec,itdfprevrec

      dimension VAL(MBITS+2)
      logical traceok,FOUND,closer

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='RCTDFB'  ! set for cfiles


C Trace output ?
      traceok = .true.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)traceok = .false.
      IF(ITRACE(3).EQ.0.OR.NSINC.GT.ITCF)traceok = .false.

C Set ISDAT to keep consistency with legacy code
      ISDAT=2

C Reset error and initialise returned VAL.
      DO 2 I=1,MBITS+2
        VAL(I)=0.0
   2  CONTINUE
      IER=0

C Workout if building side timestep is equal to tdf timestep or not
C need to do once per simulaton. Skip if any of the UK NCM models
      IF(INOTI.LT.1.OR.INOTI.GT.4)THEN
        IF(ISBNSTEP.GT.NTSPH.AND.ialrdy_warned.NE.1)THEN
          helptopic='tdf_timestep_mismatch'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('tdf warning',nbheop,'bsim-tdf-mismatch',0,0,IER)
          ialrdy_warned=1
        ENDIF
      ENDIF

C Work out which record to read for that day, IPRECT
C first determine fractional part of day to look up against FPTLA
C if we do not already know this from an earlier call.
      FPTLA=TIME/24.

C Test whether we need to calculate this again. If we do need to
C calculate, loop through each timestep in the day (yuck) and
C see if there is one which is within tollerance.
      if(iknowrec(NSINC,1).eq.0)then
        FOUND=.FALSE.
        CLOSER=.FALSE.
        IPRECT=0
        INTERPOLATE=0
        DO 12 I=1,24*NTSPH
          IF(.NOT.FOUND)THEN
            ANS=REAL(I)/REAL(24*NTSPH)-FPTLA
            call eclose(ANS,0.0,0.001,closer)
            IF(CLOSER)THEN
              IPRECT=I
              FOUND=.TRUE.
            ELSEIF(ANS.GT.0.001)THEN
              IPRECT=I
              FOUND=.TRUE.
              INTERPOLATE=1
            ENDIF
          ENDIF
 12     CONTINUE
        IF(IPRECT.EQ.0)THEN
          CALL EDISP(IUOUT,'RCTDFB : tdf read error')
        ENDIF

C Work out which day to read, IPRECD
        IF(IDYP.LT.ITDBDOY)THEN
	  IPRECD=ITDBDOY
        ELSEIF(IDYP.GE.ITDBDOY.AND.IDYP.LE.ITDEDOY)THEN
	  IPRECD=IDYP
        ELSEIF(IDYP.GT.ITDEDOY)THEN
	  IPRECD=ITDEDOY
        ENDIF

C Go to next day if 1st record for the day is to be read in order to
C avoid reading first record for that day again (but not for the last
C day)
        IF(IPRECD.NE.ITDEDOY)THEN
          IF(IPRECT.EQ.1)THEN
            IPRECD=IPRECD+1
          ELSEIF(FPTLA.LE.(1./REAL(24*NTSPH)))THEN
            IPRECD=IPRECD+1
          ENDIF
        ENDIF

C Work out which record in tdf file to read and remember in iknowrec.
        ITREC=49+(IPRECD-ITDBDOY)*24*NTSPH+IPRECT
        iknowrec(NSINC,1)=ITREC   ! record for this timestep
        iknowrec(NSINC,2)=IPRECD  ! day for this timestep
        iknowrec(NSINC,3)=IPRECT  ! timestep in the day for this timestep
        iknowrec(NSINC,4)=INTERPOLATE  ! whether to interpolate for this timestep
      else

C Use recorded indices from previous calls at this timestep.
        ITREC=iknowrec(NSINC,1)   ! restablish record for this timestep
        IPRECD=iknowrec(NSINC,2)  ! restablish day for this timestep
        IPRECT=iknowrec(NSINC,3)  ! restablish timestep in the day for this timestep
        INTERPOLATE=iknowrec(NSINC,4)  ! restablish whether to interpolate for this timestep
      endif

C Read relevant record from tdf binary / scratch file. If this is the
C same record as the last call to RCTDFB then assume that TABU is still
C got valid information.
      if(itdflastrec.eq.0.or.(itdflastrec.ne.ITREC))then
        CALL ERTABU(ITRC,ITREC,IER)
        itdflastrec=ITREC  ! remember it
      else
        continue
      endif

C Loop through each of the timestep period fields and
C stuff data from each of the fields into an array VAL.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1

C Copy appropriate fields for this single item into VAL.
      DO 66 J=ISFLDS,IEFLDS
        ITCC=J+2-ISFLDS
        VAL(ITCC)=TABU(ITCOL(IFOC,J))
   66 CONTINUE
      VAL(1)=real(IDYP)+(time/24.)

C If required to interpolate do that now and use the alternate
C data structure TABUPREV to get the previous steps data from.
      IF(INTERPOLATE.EQ.1)THEN

C Determine the interpolation factor XINTPF1 and 2
        XINTPF1=FPTLA-REAL(IPRECT-1)/REAL(24*NTSPH)
        XINTPF2=REAL(IPRECT)/REAL(24*NTSPH)-FPTLA

C Read previous record from tdf binary / scratch file via ERTABUPREV
        if(itdfprevrec.eq.0.or.(itdfprevrec.ne.ITREC-1))then
          CALL ERTABUPREV(ITRC,ITREC-1,IER)
          itdfprevrec=ITREC-1  ! remember it
        else
          continue
        endif

C Copy appropriate fields of TABUPREV into VAL.
        DO 77 J=ISFLDS,IEFLDS
          ITCC=J+2-ISFLDS
          VAL(ITCC)=(XINTPF2*TABUPREV(ITCOL(IFOC,J))+XINTPF1*VAL(ITCC))
     &            /(XINTPF1+XINTPF2)
   77   CONTINUE
      ENDIF

C Reporting.
      IF(traceok)then
        write(itu,'(a,3i4,a,3i4,a,4i4,a,4f9.3)') 
     &    'temprl gather@ idyp ihrp nsinc',idyp,ihrp,NSINC,
     &    ' itrec iprecd iprect',itrec,IPRECD,IPRECT,
     &    ' interp ifoc isflds ieflds',interpolate,ifoc,isflds,ieflds,
     &    ' tptla ans time fptla',fptla,ANS,TIME,fptla
        nss=min0(27,MBITS)
        WRITE(itu,'(a,f7.3,a,27F7.2)') 'temprl data@',time,':',
     &    (VAL(J),J=1,nss)
      ENDIF
      RETURN
      END


C ************* RCTDFBALL ************************************************
C RCTDFBALL recovers all data from the TDF binary (scratch) file at a given timestep.
C 'time' is the time (btimep or btimef) from the simulation.
C Data is put in VAL (an array sized to hold all data fields).
C ISDAT is a flag indicating which VAL index the period data begins at. 
C IDC maps fields >> VAL index where VAL(1) is period start time
C (decimal hours), VAL(2) is end, VAL(3...) are the first period fields.
C ITCC maps fields >> VAL index where VAL(1) is the fractional Julian
C day and VAL(2...) are timestep fields.

      SUBROUTINE RCTDFBALL(ITRC,time,VAL,ISDAT,IER)
#include "building.h"
#include "tdf2.h"
#include "sbem.h"
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      COMMON/TDATPREV/TABUPREV(MTABC)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/tdfrem/ialrdy_warned

C For each timestep (assuming 10 tsph frequency over a year) iknowrec
C will be zero if not yet set and a positive number representing the record
C to read if already known. 1st value is tdf record,
C  2nd value is day for this timestep,
C  3rd value is timestep in the day for this timestep,
C  4th value is whether to interpolate for this timestepX.
      integer iknowrec,itdflastrec,itdfprevrec
      common/tdfhash/iknowrec(87600,4),itdflastrec,itdfprevrec

      dimension VAL(MTABC+2)
      logical traceok,FOUND,closer

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='RCTDFBALL'  ! set for cfiles

C Trace output ?
      traceok = .true.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)traceok = .false.
      IF(ITRACE(3).EQ.0.OR.NSINC.GT.ITCF)traceok = .false.

C Set ISDAT to keep consistency with legacy code
      ISDAT=2

C Reset error and initialise returned VAL.
      DO 2 I=1,MTABC+1
        VAL(I)=0.0
   2  CONTINUE
      IER=0

C Workout if building side timestep is equal to tdf timestep or not
C need to do once per simulaton. Skip if any of the UK NCM models
      IF(INOTI.LT.1.OR.INOTI.GT.4)THEN
        IF(ISBNSTEP.GT.NTSPH.AND.ialrdy_warned.NE.1)THEN
          helptopic='tdf_timestep_mismatch'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('tdf warning',nbhelp,'bsim-tdf-mismatch',0,0,IER)
          ialrdy_warned=1
        ENDIF
      ENDIF

C Work out which record to read for that day, IPRECT
C first determine fractional part of day to look up against FPTLA
C if we do not already know this from an earlier call.
      FPTLA=TIME/24.

C Test whether we need to calculate this again. If we do need to
C calculate, loop through each timestep in the day (yuck) and
C see if there is one which is within tollerance.
      if(iknowrec(NSINC,1).eq.0)then
        FOUND=.FALSE.
        CLOSER=.FALSE.
        IPRECT=0
        INTERPOLATE=0
        DO 12 I=1,24*NTSPH
          IF(.NOT.FOUND)THEN
            ANS=REAL(I)/REAL(24*NTSPH)-FPTLA
            call eclose(ANS,0.0,0.001,closer)
            IF(CLOSER)THEN
              IPRECT=I
              FOUND=.TRUE.
            ELSEIF(ANS.GT.0.001)THEN
              IPRECT=I
              FOUND=.TRUE.
              INTERPOLATE=1
            ENDIF
          ENDIF
 12     CONTINUE
        IF(IPRECT.EQ.0)THEN
          CALL EDISP(IUOUT,'RCTDFB : tdf read error')
        ENDIF

C Work out which day to read, IPRECD
        IF(IDYP.LT.ITDBDOY)THEN
	  IPRECD=ITDBDOY
        ELSEIF(IDYP.GE.ITDBDOY.AND.IDYP.LE.ITDEDOY)THEN
	  IPRECD=IDYP
        ELSEIF(IDYP.GT.ITDEDOY)THEN
	  IPRECD=ITDEDOY
        ENDIF

C Go to next day if 1st record for the day is to be read in order to
C avoid reading first record for that day again (but not for the last
C day)
        IF(IPRECD.NE.ITDEDOY)THEN
          IF(IPRECT.EQ.1)THEN
            IPRECD=IPRECD+1
          ELSEIF(FPTLA.LE.(1./REAL(24*NTSPH)))THEN
            IPRECD=IPRECD+1
          ENDIF
        ENDIF

C Work out which record in tdf file to read and remember in iknowrec.
        ITREC=49+(IPRECD-ITDBDOY)*24*NTSPH+IPRECT
        iknowrec(NSINC,1)=ITREC   ! record for this timestep
        iknowrec(NSINC,2)=IPRECD  ! day for this timestep
        iknowrec(NSINC,3)=IPRECT  ! timestep in the day for this timestep
        iknowrec(NSINC,4)=INTERPOLATE  ! whether to interpolate for this timestep
      else

C Use recorded indices from previous calls at this timestep.
        ITREC=iknowrec(NSINC,1)   ! restablish record for this timestep
        IPRECD=iknowrec(NSINC,2)  ! restablish day for this timestep
        IPRECT=iknowrec(NSINC,3)  ! restablish timestep in the day for this timestep
        INTERPOLATE=iknowrec(NSINC,4)  ! restablish whether to interpolate for this timestep
      endif

C Read relevant record from tdf binary / scratch file. If this is the
C same record as the last call to RCTDFB then assume that TABU is still
C got valid information.
      if(itdflastrec.eq.0.or.(itdflastrec.ne.ITREC))then
        CALL ERTABU(ITRC,ITREC,IER)
        itdflastrec=ITREC  ! remember it
      else
        continue
      endif

C Loop through each of the timestep period fields and
C stuff data from each of the fields into an array VAL.
      IEFLDS=NWPR
      ISFLDS=1

C Copy fields from TABU into VAL.
      DO 66 J=ISFLDS,IEFLDS
        VAL(J)=TABU(J)
   66 CONTINUE

C If required to interpolate do that now and use the alternate
C data structure TABUPREV to get the previous steps data from.
      IF(INTERPOLATE.EQ.1)THEN

C Determine the interpolation factor XINTPF1 and 2
        XINTPF1=FPTLA-REAL(IPRECT-1)/REAL(24*NTSPH)
        XINTPF2=REAL(IPRECT)/REAL(24*NTSPH)-FPTLA

C Read previous record from tdf binary / scratch file via ERTABUPREV
        if(itdfprevrec.eq.0.or.(itdfprevrec.ne.ITREC-1))then
          CALL ERTABUPREV(ITRC,ITREC-1,IER)
          itdfprevrec=ITREC-1  ! remember it
        else
          continue
        endif

C Copy appropriate fields of TABUPREV into VAL.
        DO 77 J=ISFLDS,IEFLDS
          VAL(J)=(XINTPF2*TABUPREV(J)+XINTPF1*VAL(J))
     &            /(XINTPF1+XINTPF2)
   77   CONTINUE
      ENDIF

C Reporting.
      IF(traceok)then
        write(itu,'(a,3i4,a,3i4,a,3i4,a,4f9.3)') 
     &    'temprl all@ idyp ihrp nsinc',idyp,ihrp,NSINC,
     &    ' itrec iprecd iprect',itrec,IPRECD,IPRECT,
     &    ' interp isflds ieflds',interpolate,isflds,ieflds,
     &    ' tptla ans time fptla',fptla,ANS,TIME,fptla
        nss=min0(27,MTABC)
        WRITE(itu,'(a,f7.3,a,27F7.2)') 'temprl data@',time,':',
     &    (VAL(J),J=1,nss)
      ENDIF
      RETURN
      END
