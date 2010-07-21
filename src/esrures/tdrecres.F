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
C The version below is for use with res.

C ************* RCTDFR ************************************************
C RCTDFR recovers data from the TDF binary (scratch) file for a named instance
C at a given timestep for use in res.
C STAG is the unique id string for the instance in the db. 'time' is the
C time (btimep or btimef) from the simulation.
C Data is put in VAL (an array sized to hold data fields).
C IFOC is the instance array index within the file.
C IJ is the timestep during this day.
C ISDAT is a flag indicating which VAL index the period data begins at. 
C IDC maps fields >> VAL index where VAL(1) is period start time
C (decimal hours), VAL(2) is end, VAL(3...) are the first period fields.
C ITC maps fields >> VAL index where VAL(1) is the fractional julain
C day and VAL(2...) are timestep fields.

      SUBROUTINE RCTDFR(ITRC,ID,IJ,time,VAL,ISDAT,IFOC,IER)
#include "building.h"
#include "tdf2.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/TDAT/TABU(MTABC),TABT(MTABC),TABUMN(MTABC),TABUMX(MTABC)

      dimension VAL(MBITS+2)
      character OUTS*124
      logical unixok

C Reset error and initialise returned VAL.
      DO 2 I=1,MBITS+2
        VAL(I)=0.0
   2  CONTINUE
      IER=0

C If tabular convert simulation increment into a record, itime is
C current timestep in the day, rdoty is the julian day fraction.
C << Implement this check for within the simulation period as
C << a separate call made during the input section.
      IP=(ID-ISDS)*NTS*24+IJ
      RDOTY=ID+(time/24.)
      WRITE(OUTS,'(A,2I4,2F8.3)')' ID IP time RDOTY',
     &             ID,IP,time,RDOTY
      IF(ITRC.GT.1)CALL EDISP(IUOUT,OUTS)

C Loop through each of the timestep period fields and
C stuff data from each of the fields into an array VAL.
      IEFLDS=NTBITS(IFOC)+NTSTAT(IFOC)
      ISFLDS=IEFLDS-NTBITS(IFOC)+1

C Scan TAB columns. Find difference between start of simulation and
C the start of the TDF db.  If positive then need to add an offset to
C the 'IP'.  
      idshift = ISDS - itdbdoy

C Debug.
C      write(6,*)'isds itdbdoy itdedoy itcnst idshift ID IJ ',isds,
C     &  itdbdoy,itdedoy,idshift,ID,IJ

      ISDAT=2
      VAL(1)=RDOTY

C Scan one timesteps data from TDF file.
      if(ID.lt.itdbdoy)then
        continue
      elseif(ID.gt.itdedoy)then
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
        if(ID.lt.itdbdoy)then
          VAL(ITC)=0.
        elseif(ID.gt.itdedoy)then
          VAL(ITC)=0.
        else
          if(idshift.eq.0)then
            VAL(ITC)=TABU(ITCOL(IFOC,J))
          elseif(idshift.gt.0)then
            VAL(ITC)=TABU(ITCOL(IFOC,J))
          elseif(idshift.lt.0)then
            call edisp(iuout,'idshift < 0... ')
            VAL(ITC)=TABU(ITCOL(IFOC,J))
          endif
        endif
   66 CONTINUE
      IF(ITRC.GT.1)CALL EDISP(IUOUT,' Day | Field data...')

C Reporting.
      call isunix(unixok)
      IF(ITRC.GT.1.and.unixok)THEN
        WRITE(6,'(27F7.2)')(VAL(J),J=1,MBITS)
      ENDIF

      RETURN
      END

C ******************** MOMEASURED ********************

C MOMEASURED sets up data from temporal file for measured
C zone db temperatures, surface temperatures and (?). 
C This is done by recovering the and interpolating as required. 
C Returns requested value in GVAL.

      subroutine momeasured(focus,iday,izone,isurf)
#include "building.h"
#include "tdf2.h"

C Parameters
      character focus*8  ! key word for the focus
      integer iday  ! the current day
      integer izone ! the associated zone
      integer isurf ! the associated surface

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/AVRAGE/IDAVER

      DIMENSION VAL(MBITS+2)
C      CHARACTER OUTS*124   ! uncomment if debug needed
      LOGICAL found


C Check to see if there is a temporal measured data
C entry for this zone.
      found = .false.
      if(focus(1:5).eq.'DBZNM')then
      if(IDBTZNM(izone).ne.0)found = .true.
      elseif(focus(1:5).eq.'SURTM')then
        if(ISURTMEAS(izone,isurf).ne.0)found = .true.
      endif
      if(found)then

C For TDF db purposes the time is equivalent to btimef in bmatsv.f.
C The past timestep C is TF = TF - 1./real(NTS). 
C TFL is is the last timestep of the previous day.
        IJ=0
        DO 10 I=1,24
          I1=I-1
          T1=FLOAT(I1)
          DO 20 J=1,NTS
            IJ=IJ+1
            Y1=FLOAT(J)
            Y2=FLOAT(NTS)
            T=T1+Y1/Y2
            IF(IDAVER.EQ.0)T=T-(1.0/(Y2*2.0))
            TF = real(IJ)/Y2 
            TP = TF - (1./Y2)

C Get value of the temporal item.
            if(focus(1:5).eq.'DBZNM')then
            itdi=IDBTZNM(izone)
            elseif(focus(1:5).eq.'SURTM')then
              itdi=ISURTMEAS(izone,isurf)
            endif
            IFOC=itdi
            if(ij.eq.1)then
              CALL RCTDFR(0,IDAY,IJ,TP,VAL,ISD,IFOC,IER)
            else
              CALL RCTDFR(0,IDAY,IJ-1,TP,VAL,ISD,IFOC,IER)
            endif
            X1a=VAL(ISD)
            CALL RCTDFR(0,IDAY,IJ,TF,VAL,ISD,IFOC,IER)
            X2a=VAL(ISD)
            GVAL(IJ)=X1a+(((T-TP)*(X2a-X1a))/(TF-TP))

C Debug...
            call DATIME(IJ,TIME)
C          WRITE(OUTS,'(A,I3,4F6.2,I4,F8.2)') 'IJ T TP TF ID Va ',
C     &      IJ,T,TP,TF,TIME,IDAY,GVAL(IJ)
C            CALL EDISP(IUOUT,OUTS)
   20     CONTINUE
   10   CONTINUE
      else
        call edisp(iuout,'No measured data for this zone')
      endif

      RETURN
      END
