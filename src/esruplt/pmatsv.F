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

C This file contains the following routines:
C      MZNUMA

c ******************** MZNUMA ********************

c MZNUMA is the main controller for the numerical simulation

      SUBROUTINE MZNUMA
#include "plant.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT

      COMMON/C6/INDCFG

      COMMON/PREC7/ITCNST

      COMMON/PWRFLG/IPWRSIM

      COMMON/PCTSTP/NTSTPP
      COMMON/PCTINC/IPTS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SHAD1/IML

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/CLIM/IDIF(MT),ITMP(MT),IDNR(MT),IVEL(MT),IDIR(MT),
     &IHUM(MT),IDIFF,ITMPF,IDNRF,IVELF,IDIRF,IHUMF
      COMMON/SUS/ISSNY

      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/Pctime/TIMSEC
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax
      common/tsconp/iptsc,nptscp,ptsdat(24,20)
      common/outflg/itsplt
      COMMON/PSTCTR/NSINCP,NSNCPR

C Simulation start day required by cmp70c.
      common/simsdy/iss,isf

      character outs*124

C Expected finish time variables.
            integer time
      character*24 ectime

      integer ratiop

C CWEC Solar radiation fix
C These values are needed for interpolation if
C solar radiation data are half-hour centered.
      COMMON/CWEC_SOL2/idif_next,idnr_next
      integer  idif_next, idnr_next         !- diffuse and direct normal radiation data, respectively
                                            !- for 2nd hour of next day.


C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) then
         WRITE(outs,'(A)') ' Entering subroutine MZNUMA'
         call edisp(itu,outs)
      endif

c ISSNY determines whether or not a simulation spans the New Year
c period.
      ISSNY=0

c Write simulation control information to the solutions
c library.
      IF(INDCFG.NE.1)CALL MZPLS2

c Invoke initialisation of climate data and
c zone future air temperatures.
      CALL MZINI1

c Set simulation start and finish days: actual start day is prior
c to the requested start day (as determined in MZTCON).   This
c is necessary to eliminate the effects of the assumed starting
c conditions.
      ISS=ISDS-ITCNST
      ISF=ISDF
      NHS=(ISDF-ISDS+1+ITCNST)*24
      INHS=0
      INHSI=0

c Has the start day been converted from a post-new year day to
c a pre-new year day.
      IF(ISS.LT.1)GOTO 1
      GOTO 2
    1 ISS=365-ITCNST+ISDS
      ISF=ISF+365
      ISSNY=1

c Determine simulation year from climate file.
    2 IUNIT=IFIL
      IREC=366
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)IYEAR

      CALL MZSIMH(ICON)

c Terminate simulation if ICON=0.
      IF(ICON.EQ.0)GOTO 16
      IML=0
      NSINC=0

c Initialise plant time-step controller variables.
      itstpp=ntstpp
      otimsc=timsec
      ratiop=1
      iterp=0
      itctyp=0

c If monitoring is on then while in start-up period, display message .
      if(monitr.eq.1.and.itcnst.gt.0)
     & call usrmsg(' ',
     &     ' Monitoring will commence shortly, please wait...','-')

C Remember the 'start value' of the UNIX system clock.
      ISTIME=TIME()
      ILTIME=ISTIME

c Start simulation.
      DO 10 I=ISS,ISF

c Initialise period counter for plant time-step controller.
      nptspd=1

c Set day 'I' (the present day) and day 'I+1' (the
c future day) actual day numbers.
      IDYP=I
      IDYF=I+1
      IF(IDYP.GT.365)GOTO 3
      IF(IDYF.EQ.366)IDYF=1
      GOTO 4
    3 IDYP=IDYP-365
      IDYF=IDYP+1
      IF(IDYF.EQ.366)IDYF=1

c What day of week is present and future day: Monday 1
c through Sunday 7.
    4 CALL EDAYR(IDYP,ID,IM)
      CALL EWEEKD(ID,IM,IYEAR,IDWP)
      CALL EDAYR(IDYF,ID,IM)
      CALL EWEEKD(ID,IM,IYEAR,IDWF)

c Establish climatic data for present day and first
c hour of future day: future day first.
      IREC=IDYF
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IDIF(J),ITMP(J),
     &IDNR(J),IVEL(J),IDIR(J),IHUM(J),J=1,24)
      IDIFF=IDIF(1)
      ITMPF=ITMP(1)
      IDNRF=IDNR(1)
      IVELF=IVEL(1)
      IDIRF=IDIR(1)
      IHUMF=IHUM(1)

C Get values for next day to determine solar radiation data
C at hour 1 of next day (needed if solar radiation data is half-hour centered).
      idif_next = idif(2)
      idnr_next = idnr(2)

c Present day.
      IREC=IDYP
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(IDIF(J),ITMP(J),
     &IDNR(J),IVEL(J),IDIR(J),IHUM(J),J=1,24)

c Continue simulation on an hour-by-hour basis.
      DO 20 J=1,24

c Set present and future hours.
      IHRP=J
      IHRF=J+1
      IF(IHRF.EQ.25)IHRF=1

c If plant TSC is active then call tsctyp to get a TSC type
c (if any) for the present time.
      if(iptsc.eq.1) call tsctyp(ihrp,itctyp,nptspd)

C ITSCF1 controls the climate look ahead as required
C by a type 1 time-step controller. If 1 it will
C ensure that the controller receives climate data for
C a time-row 1 time-step in advance of the numerical
C simulation time-row.
c *** CHECK *** ITSCF1 unreferenced
      ITSCF1=0

c Is building computational time-step less than one hour ?
      N=NTSTEP
      DO 30 K=1,N

C ITS is the current time-step within an hour.
      ITS=K

C Increment the simulation counter
      NSINC=NSINC+1

C Set the future time-row hour value.
      BTIMEP=BTIMEF
      IF(NSINC.EQ.1)BTIMEP=1.0
      BTIMEF=FLOAT(IHRP)+FLOAT(ITS)/FLOAT(NTSTEP)
      IF(BTIMEF.GT.24.0)BTIMEF=BTIMEF-24.

C Output current simulation increment, if TRACE ON.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(33).NE.0) then
         write(outs,'(A,I3)')' Simulation increment ',nsinc
         call edisp(itu,outs)
      endif

C Determine climatic parameters at the present and
C future time row.
      CALL MZCLMPT(1)

C First time step only: assign starting values of plant nodal variables
      IF(I.EQ.ISS.AND.J.EQ.1.AND.K.EQ.1)THEN
         CALL MZNASP

C Initialise plant time-step counters.
         NSINCP=0
         NSNCPR=0

C Initialise control parameters as required by pid controllers.
         CALL MZIPCT
       ENDIF

c If plant TSC is of type 1, then find current time-step value.
c Also return the ratio of new 'ntstpp' over old 'ntstpp'(itstpp).
  707 if(itctyp.eq.1) call ptsc1(itstpp,ratiop,nptspd)

C Plant multiple time-steps within each building-side time-step.
      DO 100 KK=1,NTSTPP

C Adjust timer
      IPTS=KK
      PTIMEP=PTIMEF
      IF(NSINC.EQ.1.AND.KK.EQ.1)PTIMEP=1.0
      ITS0=ITS-1
      PTIMEF=FLOAT(IHRP)+FLOAT(ITS0)/FLOAT(NTSTEP)
     &+FLOAT(IPTS)/FLOAT(NTSTPP*NTSTEP)
      IF(PTIMEF.GT.24.0)PTIMEF=PTIMEF-24.0

C Increment plant time-step counter.
      NSINCP=NSINCP+1

C Assign climate variable values
      CALL MZCLMPT(2)

C Set up and solve the plant matrix
      CALL MZPMRX(ITERA)

C Could use the variable ITERA if an iteration procedure
C was implemented. For example if ITERA was not zero, then
C repeat the previous calculation sequence.

C Store plant results if not in start-up period and if user
c specified time-step was reached.
      iday=idyp

c Set itsplt if output is required for this time-row
      itsplt=0
      if(amod(float(kk),float(ratiop)).eq.0.0) then
         call mzpls3(iday,iflag)
         itsplt=1
         if(itctyp.ne.2.and.monitr.eq.1.and.iflag.eq.1)
     &     call mgraph(0,0,itstpp,2)
      endif

  100 continue

c For plant TSC of type 2 check if criteria is statisfied.
c If not reduce time-step and iterate.
      if(itctyp.eq.2) then
         call ptsc2(itstpp,ratiop,iterp)
         if(iterp.ne.0) goto 707

c For now ignore monitoring of state variables for this
c type.
c         if(monitr.eq.1) call mgraph(0,0,2)
      endif




C Plant side now complete
   30 CONTINUE

c Assign old plant time-step value and initialise other variables.
      ntstpp=itstpp
      timsec=otimsc
      iterp=0
      ratiop=1
      itctyp=0


C Hour step now complete
      CONTINUE

C Establish percentage completion (of simulation), adjust expected
C finish time and output appropriate message but only if previous
C report was issued more than 5 seconds ago.
      INHS=INHS+1
      SIMCOM=FLOAT(INHS)/FLOAT(NHS)
      Ictime=TIME()
      ISTAMP=0

      IF(SIMCOM.GT.0.95.AND.INHSI.LT.6) THEN
         INHSI=INHSI+1
         IPERC=95
         ISTAMP=1
      ELSE IF(SIMCOM.GT.0.75.AND.INHSI.LT.5) THEN
         INHSI=INHSI+1
         IPERC=75
         ISTAMP=1
      ELSE IF(SIMCOM.GT.0.50.AND.INHSI.LT.4) THEN
         INHSI=INHSI+1
         IPERC=50
         ISTAMP=1
      ELSE IF(SIMCOM.GT.0.25.AND.INHSI.LT.3) THEN
         INHSI=INHSI+1
         IPERC=25
         ISTAMP=1
      ELSE IF(SIMCOM.GT.0.05.AND.INHSI.LT.2) THEN
         INHSI=INHSI+1
         IPERC=5
         ISTAMP=1
      ELSE IF(SIMCOM.GT.0.01.AND.INHSI.LT.1) THEN
         INHSI=INHSI+1
         IPERC=1
         ISTAMP=1
      END IF

      IF(ISTAMP.NE.0.AND.(Ictime-ILTIME).GE.5) THEN
         ILTIME=Ictime
         ietim=istime+(ictime-istime)*100/iperc
         write(outs,'(I4,A,A)') iperc,
     &      ' %   complete; expected finish time : ',
     &      ectime(ietim)
         call edisp(iuout,outs)
      END IF

   20 CONTINUE
C Hour loop now complete.

   10 CONTINUE
C Day loop now complete.

c Set start address of next solution-set in
c library.
      IF(INDCFG.NE.1)CALL MZPLS4

c Disable plant time-step controller.
      iptsc=0

   16 if(itc.gt.0.and.nsinc.ge.itc.and.nsinc.le.itcf.and.
     &   itrace(40).ne.0) call edisp(itu,
     &                               ' Leaving subroutine mznuma.')

C Inform user that simulation complete before returning.
      call usrmsg(' Simulation now complete.',
     &            ' Remember to SAVE your results!','-')
      return

 1000 write(outs,'(A,I3)') ' mznuma: climate file error @ rec ',irec
      call edisp(iuout,outs)
      CALL EPWAIT
      GOTO 16
      END
