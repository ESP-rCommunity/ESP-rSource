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
C Data is put in VAL (an array sized to hold data fields).
C IFOC is the instance array index within the file.
C ISDAT is a flag indicating which VAL index the period data begins at. 
C IDC maps fields >> VAL index where VAL(1) is period start time
C (decimal hours), VAL(2) is end, VAL(3...) are the first period fields.
C ITC maps fields >> VAL index where VAL(1) is the fractional Julian
C day and VAL(2...) are timestep fields.

      SUBROUTINE RCTDFB(ITRC,time,VAL,ISDAT,IFOC,IER)
#include "building.h"
#include "tdf2.h"
      common/pophelp/h(60)
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/tdset/itdyear,itdbdoy,itdedoy
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      dimension VAL(MBITS+2)
      logical traceok,FOUND,closer
      character h*72

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
C need to do once per simulaton
      IF(ISBNSTEP.GT.NTSPH.AND.NSINC.EQ.1)THEN
        h(1)='tdf - simulation timestep mismatch'
        h(2)='You are simulating with time step greater'
        h(3)='than that at which measured data is recorded'
        h(4)='This can cause errors in results!'
        CALL PHELPD('tdf warning ',4,'bsim-tdf-mismatch',0,0,IER)
      ENDIF

C Work out which record to read for that day, IPRECT
c first determine fractional part of day to look up against FPTLA
      FPTLA=TIME/24.
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
 12   CONTINUE
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

C Work out which record in tdf file to read
      ITREC=49+(IPRECD-ITDBDOY)*24*NTSPH+IPRECT

C Read relevant record from tdf binary / scratch file
      CALL ERTABU(ITRC,ITREC,IER)

C Loop through each of the timestep period fields and
C stuff data from each of the fields into an array VAL.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1

C Copy appropriate fields into VAL.
      DO 66 J=ISFLDS,IEFLDS
        ITC=J+2-ISFLDS
        VAL(ITC)=TABU(ITCOL(IFOC,J))
   66 CONTINUE
      VAL(1)=real(IDYP)+(time/24.)

C If required to interpolate do that now
      IF(INTERPOLATE.EQ.1)THEN

C Determine the interpolation factor XINTPF1 and 2
        XINTPF1=FPTLA-REAL(IPRECT-1)/REAL(24*NTSPH)
        XINTPF2=REAL(IPRECT)/REAL(24*NTSPH)-FPTLA

C Read relevant record from tdf binary / scratch file
        CALL ERTABU(ITRC,ITREC-1,IER)

C Copy appropriate fields into VAL.
        DO 77 J=ISFLDS,IEFLDS
          ITC=J+2-ISFLDS
          VAL(ITC)=(XINTPF2*TABU(ITCOL(IFOC,J))+XINTPF1*VAL(ITC))
     &            /(XINTPF1+XINTPF2)
   77   CONTINUE
      ENDIF

C Debug...
C      write(96,'(6i4,1x,f7.5,1x,f8.3)')!1x,F7.3)')!,x,f6.4,x,f6.4,x,f8.2,x,f4.1)')
C     &idyp,ihrp,itrec,IPRECD,IPRECT,interpolate,
C     &ifoc,isflds,ieflds,!ITCOL(IFOC,3),ITCOL(IFOC,4),
C     &fptla,ANS,TIME
C     &fptla,val(4)
C      write(97,'(4F12.6)')VAL(1),val(2),val(3),val(4)

C Reporting.
      IF(traceok)then
        nss=min0(27,MBITS)
        WRITE(itu,'(27F7.2)')(VAL(J),J=1,nss)
      ENDIF
      RETURN
      END

