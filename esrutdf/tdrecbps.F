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
C ITC maps fields >> VAL index where VAL(1) is the fractional julain
C day and VAL(2...) are timestep fields.

      SUBROUTINE RCTDFB(ITRC,time,VAL,ISDAT,IFOC,IER)
#include "building.h"
#include "tdf2.h"
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      common/prec7/itcnst
      common/tdset/itdyear,itdbdoy,itdedoy
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFIH2/NTSTAT(MIT),NTBITS(MIT),
     &              NTMAR(MIT),NTMAT(MIT),NTMAI(MIT)
      COMMON/TDFLD2/ITDFTR(MIT,MBITS),IATDF(MIT,MBITS),ITCOL(MIT,MBITS)
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

      dimension VAL(MBITS+2)
      logical traceok
      character OUTS*160

C Trace output ?
      traceok = .true.
      IF(ITC.LE.0.OR.NSINC.LT.ITC)traceok = .false.
      IF(ITRACE(3).EQ.0.OR.NSINC.GT.ITCF)traceok = .false.

C Reset error and initialise returned VAL.
      DO 2 I=1,MBITS+2
        VAL(I)=0.0
   2  CONTINUE
      IER=0

C If simulation day is before the db period then return zeros, if
C after return zeros.  NSINC = 1 is the first timestep after 1h00
C on the startup day (e.g. 2h00). To compensate for this set IP
C to one more than nsinc if there is one timestep per hour for
C any hour after nsinc=1.
      if(NTSPH.eq.1)then
        if(nsinc.eq.1)then
          IP=1
        else
          IP= NSINC+1
        endif
      elseif(NTSPH.gt.1)then
        IP= NSINC + NTSPH
      endif
      if(IDYP.lt.itdbdoy)then
        call edisp(iuout,'Day before TDF data - returning zeros. ')
      elseif(IDYP.gt.itdedoy)then
        call edisp(iuout,'Day after TDF data - returning zeros. ')
      endif

C If tabular convert simulation increment into a record, itime is
C current timestep in the day, rdoty is the julian day fraction.
      RDOTY=real(IDYP)+(time/24.)
      WRITE(OUTS,'(A,3I4,3F8.3)')
     &  ' Temporal trace: IDYP NSINC IP time day btimef',
     &  IDYP,NSINC,IP,time,RDOTY,BTIMEF
      if(traceok)then
        CALL EDISP(itu,OUTS)
        CALL EDISP(itu,' Date   Temporal Data...')
      endif

C Loop through each of the timestep period fields and
C stuff data from each of the fields into an array VAL.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1

C Scan TAB columns. Find difference between start of simulation and
C the start of the TDF db.  If positive then need to add an offset to
C the 'IP'.
      idshift = (ISDS - itcnst) - itdbdoy
      ISDAT=2
      VAL(1)=RDOTY

C Scan one timesteps data from TDF file.
      if(IDYP.lt.itdbdoy)then
        continue
      elseif(IDYP.gt.itdedoy)then
        continue
      else
        if(idshift.eq.0)then
          ITREC=IP+49
          CALL ERTABU(ITRC,ITREC,IER)
        elseif(idshift.gt.0)then
          IXP=IP+(idshift*NTSPH*24)
          ITREC=IXP+49
          CALL ERTABU(ITRC,ITREC,IER)
        elseif(idshift.lt.0)then
          call edisp(iuout,'idshift < 0... ')
          IXP=IP-(idshift*NTSPH*24)
          ITREC=IXP+49
          CALL ERTABU(ITRC,ITREC,IER)
        endif
      endif

C Copy appropriate fields into VAL.
      DO 66 J=ISFLDS,IEFLDS
        ITC=J+2-ISFLDS
        if(IDYP.lt.itdbdoy)then
          VAL(ITC)=0.
        elseif(IDYP.gt.itdedoy)then
          VAL(ITC)=0.
        else
          if(idshift.eq.0)then
            VAL(ITC)=TABU(ITCOL(IFOC,J))
          elseif(idshift.gt.0)then
            VAL(ITC)=TABU(ITCOL(IFOC,J))
          elseif(idshift.lt.0)then
            VAL(ITC)=TABU(ITCOL(IFOC,J))
          endif
        endif
   66 CONTINUE

C Reporting.
      IF(traceok)then
        nss=min0(27,MBITS)
        WRITE(itu,'(27F7.2)')(VAL(J),J=1,nss)
      ENDIF

      RETURN
      END

