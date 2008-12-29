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
C      TSCINP
C      TSCON1
C      TSCON2
C      TSCON4
C      TSCON6
C      TSCON7
C      COEFF1

C ******************** TSCINP ********************
C TSCINP is called by MZSIML for specifying number of
C TSC periods and TSC type for each period.
C
C ITSFLG       : Time-step controlller flag.
C NTSCPD       : Number of distinct TSC periods during a day.
C ITSSCH(?,1)  : Start hour of each control period.
C       (?,2)  : Finish hour of each control period.
C       (?,4)  : Start day of each control period.
C       (?,5)  : Start time-step of each control period.
C       (?,6)  : Finish day of each control period.
C       (?,3)  : Time-step controller type for each period. The
C                following types are allowed :
C                     0 : No time-step control.
C                     1 : Look-ahead on weather variables before using
C                         new time-step value.
C                     2 : Time-step is successively halfed until
C                         condition set on node air temperature and/or
C                         energy injection is met.
C                     3 : Time-step value is user-specified.
C                     4 : Iterate without changing time-step value.
C                     6 : Rewind to period start time.
C itsmus       : If set then transfer of results at the current time-
C                step is allowed.
C ITSCPV       : Current period value.
C ITSCFN       : Current control variable.

C TYPE 1 TSC variables...
C NF           : Number of control variables.
C IFL          : Defines type of each control variable as follows:
C                   1 : Outside dry bulb temperature.
C                   2 : Direct normal solar radiation.
C                   3 : Diffuse horizontal solar radiation.
C                   4 : Wind speed.
C                   5 : Wind direction.
C                   6 : Relative humidity.
C FD           : Max difference allowed for each control variable.
C                This is the difference between future time-row and
C                present time-row value of control variable at current
C                time-step.
C NMAX1        : Max number of time-step reductions allowed to reach
C                difference specified for control variable(s). This
C                difference may be reached before NMAX1.

C TYPE 2 TSC variables...
C NF2          : Number of control variables.
C IFL2         : Defines type of each control variable as follows:
C                   1 : Zone air temperature.
C                   2 : Zone air node energy injection.
C FD2          : Max difference allowed for each control variable.
C                This is hourly average difference between current
C                time-step and previous time-step value for the same
C                hourly interval.
C NMAX2        : Max number of time-step reductions allowed within
C                each hour in the run period to reach difference range
C                specified for control variable(s).
C TASAV        : Sums the averages evaluated for the zone air temperature.
C                These averages are evaluated at each timestep within
C                the hour.
C QASAV        : Sums the averages evaluated for the zone air energy
C                injection at each timestep within the hour.

C TYPE 3 TSC variables...
C ITSV         : User specified time-step value.

C Type 4 TSC variables...
C ITSITR       : Number of iterations to be performed for an hour interval.
C ITRPAS       : Counts number of iterations performed so far.

      SUBROUTINE TSCINP
#include "building.h"

      common/pophelp/h(60)
      COMMON/TS/ITSFLG,NTSCPD,ITSSCH(MSCH,5),itsmus
      COMMON/TS1/NF(MSCH),IFL(MSCH,MCVT1),FD(MSCH,MCVT1),ITSCF1,
     &           NMAX1(MSCH)
      COMMON/TS2/NF2(MSCH),IFL2(MSCH,MCVT2),FD2(MSCH,MCVT2),
     &           NMAX2(MSCH),IPASS,IPASSC,TASAV(MCOM),QASAV(MCOM)
      COMMON/TS3/ITSV(MSCH)
      COMMON/TS4/ITSITR(MSCH),ITRPAS
      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone

      COMMON/TSCIO/ITSCPV,ITSCFN
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
     
      logical tsclaw,rewnd,pasone
      
      dimension icv1f(MCVT1), icv2f(MCVT2)

      CHARACTER*39  ZITEM(9)
      character*36  zitem1(9)
      character     H*72,outs*80

C Initialise timestep controllers variables.
    1 ITSFLG=1
      NTSCPD=1
      IPASS=0
      DO 400 I=1, MSCH
         NF(I)=0
         NF2(I)=0
         NMAX1(I)=0
         NMAX2(I)=0
         ITSV (I)= 1
         DO 410 J=1, MCVT1
            IFL(I,J)=0
            FD (I,J)=0.0
  410    CONTINUE
         DO 405 J=1, MCVT2
            IFL2(I,J)=0
            FD2 (I,J)=0.0
  405    CONTINUE
         DO 400 J=1, 3
            ITSSCH(I,J) = 0
  400 CONTINUE
      jpick=0

C Build main menu for available time-step controllers.
  210 zitem(1)='0 No time-step control'
      zitem(2)='1 Boundary condition look ahead'
      zitem(3)='2 time-step reduction by iteration'
      zitem(4)='3 User specified time-step value'
      zitem(5)='4 Iteration without time-step reduction'
      zitem(6)='6 Rewind to period start time'
      zitem(7)='  ....................'
      zitem(8)='? help'
      zitem(9)='- Exit'

C Display menu.
      NITEMS = 9
      INO = -5
      call usrmsg(' ','Select time-step controller type from menu.','-')
      CALL EMENU('Time-step controllers',ZITEM,NITEMS,INO)
      if(ino.ge.1.and.ino.le.5.and.jpick.eq.1) then
         ntscpd=ntscpd+1
         jpick=0
      endif

C For each TSC, get period start time.
C Consider case for time-step controller type 0.
C Type 0 imposes no control.
      if(ino.eq.1) then
  120    CALL EASKI(istart,' ',' Period start hour ?',
     &       0,'F',24,'F',0,'start hour',IER,1)
         if(ier.ne.0) goto 120
         itssch(ntscpd,3)=0
         itssch(ntscpd,1)=istart
         goto 210

C Consider case for time-step controller type 1.
C Type 1 is the boundary condition look ahead.
      elseif(ino.eq.2) then
      do 404 i=1, MCVT1
         icv1f(i)=0
  404 continue
  130    CALL EASKI(istart,' ',' Control period start hour ?',
     &       0,'F',24,'F',0,'start hour',IER,1)
         if(ier.ne.0) goto 130
         nmax1(ntscpd)=10
         itssch(ntscpd,3)=1
         itssch(ntscpd,1)=istart

C Build main menu.
C Menu contains available control variables.
 1030 zitem1(1)='1 Outside dry bulb temperature'
      zitem1(2)='2 Direct normal solar radiation'
      zitem1(3)='3 Diffuse horizontal solar radiation'
      zitem1(4)='4 Wind speed'
      zitem1(5)='5 Wind direction'
      zitem1(6)='6 Relative humidity.'
      zitem1(7)='  ....................'
      zitem1(8)='- Exit'

C Display menu.
      NITEMS = 8
      INO = -5
      call usrmsg(' ',' Select control variable from menu.','-')
      CALL EMENU('Control variables for TSC 1',
     &    ZITEM1,NITEMS,INO)

C For each selected control variable read user specified
C rate of change tolerance value.
      if(ino.ge.1.and.ino.le.6) then

C Set selected control variable flag.
C Increment number of control variables counter.
          if(icv1f(ino).eq.0) then
            nf(ntscpd)=nf(ntscpd)+1
            icv1f(ino)=nf(ntscpd)
            ifl(ntscpd,nf(ntscpd))=ino
            mitem=nf(ntscpd)
         else
            mitem=icv1f(ino)
         endif
 1041    value=fd(ntscpd,mitem)
        CALL EASKR(value,' ',
     &    ' Rate of change tolerance value for control variable ?',
     &       0.0,'F',100000.,'F',1.,'rate of change',IER,1)
         if(ier.ne.0) goto 1041
         fd(ntscpd,mitem)=value
         jpick=1
         goto 1030
      endif

C Return to main time-step controllers menu.
      if(ino.eq.8) goto 210
      goto 1030

C Consider case for time-step controller type 2.
C Type 2 is the time-step reduction by iteration.
      elseif(ino.eq.3) then
      do 406 i=1, MCVT2
         icv2f(i)=0
  406 continue
  140    CALL EASKI(istart,' ',' Period start hour ?',
     &       0,'F',24,'F',0,'start hour',IER,1)
         if(ier.ne.0) goto 140
  145    itrdef=10
         ivalue=nmax2(ntscpd)
         if(ivalue.eq.0) ivalue=itrdef
         CALL EASKI(ivalue,' ',' Maximum number of iterations (>2) ?',
     &       2,'F',100,'F',itrdef,'no of iterations',IER,1)
         if(ier.ne.0) goto 145
         nmax2(ntscpd)=ivalue
         itssch(ntscpd,3)=2
         itssch(ntscpd,1)=istart

C Build main menu.
C Menu contains available control variables.
 1045 zitem1(1)='1 Zone air temperature'
      zitem1(2)='2 Zone air node energy injection'
      zitem1(3)='  ....................'
      zitem1(4)='- Exit'

C Display menu.
      NITEMS = 4
      INO = -5
      call usrmsg(' ',' Select control variable from menu.','-')
      CALL EMENU('Control variables for TSC 2',
     &    ZITEM1,NITEMS,INO)

      if(ino.ge.1.and.ino.le.2) then

C Set selected control variable flag.
C Increment number of control variables counter.
          if(icv2f(ino).eq.0) then
            nf2(ntscpd)=nf2(ntscpd)+1
            icv2f(ino)=nf2(ntscpd)
            ifl2(ntscpd,nf2(ntscpd))=ino
            mitem=nf2(ntscpd)
         else
            mitem=icv2f(ino)
         endif
 1047    if(ino.eq.1) dfault=1.0
         if(ino.eq.2) dfault=50.0
         value=fd2(ntscpd,mitem)
         CALL EASKR(value,' ',
     &    ' Difference allowed for control variable ?',
     &       0.0,'F',100000.,'F',dfault,'rate of change',IER,1)
         if(ier.ne.0) goto 1047
         fd2(ntscpd,mitem)=value
         jpick=1
         goto 1045
      endif
      if(ino.eq.4) goto 210
      goto 1045

C Consider case for time-step controller type 3.
C Type 3 is the user specified time-step value.
      elseif(ino.eq.4) then
  150    CALL EASKI(istart,' ',' Period start hour ?',
     &       0,'F',24,'F',0,'start hour',IER,1)
         if(ier.ne.0) goto 150
         itssch(ntscpd,3)=3
         itssch(ntscpd,1)=istart
  155    iv=ITSV(ntscpd)
         call easki(iv,' ',' Time-steps/hour for this period ?',1,'F',
     &     MTMS,'F',2,'time-steps/hour?',ier,7)
         ITSV(ntscpd)=iv
         IF (AMOD(FLOAT(ITSV(ntscpd)),FLOAT(NTSTEP)).NE.0.0) THEN
            call edisp(iuout,' Time-step must equal NTSTEP * I where:')
            call edisp(iuout,
     &        ' NTSTEP is the fixed building side time-steps/hour.')
            call edisp(iuout,' I is an integer value of 2,4,6 etc.')
           GOTO 155
         ENDIF         
         jpick=1
         goto 210

C Consider case for time-step controller type 4.
C Type 4 is iteration with no time-step reduction.
      elseif(ino.eq.5) then
  160    istart=0
         CALL EASKI(istart,' ',' Period start hour ?',
     &       0,'F',24,'F',0,'start hour',IER,1)
         if(ier.ne.0) goto 160
         itssch(ntscpd,3)=4
         itssch(ntscpd,1)=istart
  165    iterv=itsitr(ntscpd)
         if(iterv.eq.0) iterv=1
         CALL EASKI(iterv,' ',
     &       'Enter number of iterations/building time-step  ?',
     &       1,'F',500,'W',1,'iterations/time-step',IER,1)
         if(ier.ne.0) goto 165
         itsitr(ntscpd)=iterv
         jpick=1
         goto 210

C Consider case for time-step controller type 6.
C Type 6 is rewind to period start time.
      elseif(ino.eq.6) then
  170    idystr=0
         idyfin=0
         ihrstr=0
         ihrfin=0
         itsstr=0         
         itsfin=0
  
  180    CALL EASKI(idystr,' ',' Period start day ?',
     &       1,'F',365,'F',0,'start day',IER,1)  
         CALL EASKI(idyfin,' ',' Period finish day ?',
     &       1,'F',365,'F',0,'finish day',IER,1)
   
         CALL EASKI(ihrstr,' ',' Period start hour each day?',
     &       0,'F',24,'F',0,'start hour',IER,1)   
         CALL EASKI(ihrfin,' ',' Period finish hour each day ?',
     &       0,'F',24,'F',0,'finish hour',IER,1)
     
         IF(IDYSTR.GT.IDYFIN)THEN
          call edisp(iuout,' Start day later than finish day
     &  - Please re-enter period details ')
          call edisp(iuout,outs)
          GOTO 180
         ENDIF   

         if(ier.ne.0) goto 170
         
         itssch(ntscpd,1)=ihrstr
         itssch(ntscpd,2)=ihrfin
         itssch(ntscpd,3)=6
         itssch(ntscpd,4)=idystr
         itssch(ntscpd,5)=idyfin

C Set flag indicating that TSC 6 variables set from menu and 
C NOT a control law
         tsclaw=.false.

C Display help
      elseif(ino.eq.8) then
        h(1) ='You can specify a control period for the time-step'
        h(2) ='controller type you pick from the main menu.'
        h(3) ='The following is a synopsis of each type:'
        h(4) =' '
        h(5) ='0: No control imposed on time-step.'
        h(6) =' '
        h(7) ='1: Monitors user specified control variable(s) and'
        h(8) ='   reduces time-step value if rate of change of the'
        h(9) ='   control variable greater than user specified value.'
        h(10) ='   value.'
        h(11) =' '
        h(12)='2: Reduces time-step value until diference in control'
        h(13)='   variable for current time-step and previous time-'
        h(14)='   step is within user specified value.'
        h(15)=' '
        h(16)='3: Time-step value used is user specified value.'
        h(17)='   value.'
        h(18)='4: Iterate without reducing time-step value.'
        CALL PHELPD('tsc help',18,'-',0,0,IER)
        goto 210

C Return to Simul menu.
      elseif(ino.eq.9) then

C Check for overlapping periods.
         IF (NTSCPD.EQ.1) GOTO 330
         call edisp(iuout,' Checking for period overlapping.')
         I=1
         write(outs,720) I,ITSSCH(I,1)
  720    format(' Period ',I4,'   Start hour',I5)
         call edisp(iuout,outs)
         DO 310 I=2, NTSCPD
            ITSCPV=I
            write(outs,720) I,ITSSCH(I,1)
            call edisp(iuout,outs)
  315       IF (ITSSCH(I,1).LE.ITSSCH(I-1,1)) THEN
               write(outs,740) I
  740          format(' Please re-input start hour for period',I5)
               call edisp(iuout,outs)
  745          istart=0
               CALL EASKI(istart,' ',' Period start hour ?',
     &             0,'F',24,'F',0,'start hour',IER,1)
               if(ier.ne.0) goto 745
               itssch(i,1)=istart
               GOTO 315
            ENDIF
  310    CONTINUE

C Find finish hour for each period
         DO 325 I=1,(NTSCPD-1)
           if(itssch(ntscpd,3).ne.5)ITSSCH(I,2)=ITSSCH(I+1,1)
  325    CONTINUE
  330   if(itssch(ntscpd,3).ne.5)ITSSCH(NTSCPD,2)=24
        return
      endif

C Wrong item picked.
      goto 210
      
      end

C ********************** TSCON1 **********************
C TSCON1 alters the current time_step if the
C gradient of a control variable exceed the user-specified
C value. Only weather variables are used as control variables.

      SUBROUTINE TSCON1(NTSCTR)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/TS1/NF(MSCH),IFL(MSCH,MCVT1),FD(MSCH,MCVT1),ITSCF1,
     &           NMAX1(MSCH)

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      character outs*124

C LC is the loop counter. Counts number of passes required
C to achieve the user specified values for control variables

      ITSCF1=0
      LC =0
      IF (NSINC.GE.1) ITSCF1=1

C If current pass=NMAX1 then exit.
    1 IF (LC.GE.NMAX1(NTSCTR)) GOTO 15

C Determine climatic parameters at the present and
C future time row.
      ITS = 1
      CALL MZCLMPT(1)

C Loop over control variables

      DO 10 I=1, NF(NTSCTR)
         II=IFL(NTSCTR,I)

C Outside dry bulb temperature
         IF(II.EQ.1) GRAD=TF-TP

C Direct normal radiation
         IF(II.EQ.2) GRAD=QDF-QDP

C Diffuse horizontal radiation
         IF(II.EQ.3) GRAD=QFF-QFP

C Wind velocity
         IF(II.EQ.4) GRAD=VF-VP

C Wind direction
         IF(II.EQ.5) GRAD=DF-DP

C Relative humidity
         IF(II.EQ.6) GRAD=HF-HP

C If GRAD > user value then double NTSTEP and loop again
         IF(ABS(GRAD).GT.FD(NTSCTR,I)) THEN
            NTSTEP = NTSTEP * 2
            LC = LC + 1
            GOTO 1
         ENDIF
   10 CONTINUE
   15 CONTINUE

C Trace output?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)GOTO 9999
      IF(ITRACE(41).NE.1.OR.NSINC.GT.ITCF)GOTO 9999
      write(outs,'(A,I4,A,I4)')
     & ' Subroutine TSCON1     Trace output',ICNT,'  Present hour',IHRP
      call edisp(itu,outs)
      call edisp(itu,' ')
      ICNT=ICNT+1

      write(outs,'(A,I5)')
     & ' Modified building-side time-step (NTSTEP) =',NTSTEP
      call edisp(itu,outs)
      write(outs,'(A,I5)')
     & ' Passes to obtain user-specified tolerance (LC) =', LC
      call edisp(itu,outs)
      write(outs,'(A,I5)')
     & ' Maximum number of passes (NMAX1) = ', NMAX1(NTSCTR)
      call edisp(itu,outs)

      IF (LC.GE.NMAX1(NTSCTR)) THEN
        call edisp(itu,
     &   ' TSCON1: NTSTEP not modified since LC = NMAX1')
        GOTO 9999
      ENDIF

      call edisp(itu,
     & '                    Present     Future     Threshold value')
      write(outs,530) TP,TF,FD(NTSCTR,1)
  530 format(' Dry-bulb temp      :',F8.2,4X,F8.2,4X,F8.2)
      call edisp(itu,outs)
      write(outs,540) QDP,QDF,FD(NTSCTR,2)
  540 format(' Direct normal rad  :',F8.2,4X,F8.2,4X,F8.2)
      call edisp(itu,outs)
      write(outs,550) QFP,QFF,FD(NTSCTR,3)
  550 format(' Diffuse horiz rad  :',F8.2,4X,F8.2,4X,F8.2)
      call edisp(itu,outs)
      write(outs,560) VP,VF,FD(NTSCTR,4)
  560 format(' Wind velocity      :',F8.2,4X,F8.2,4X,F8.2)
      call edisp(itu,outs)
      write(outs,570) DP,DF,FD(NTSCTR,5)
  570 format(' Wind direction     :',F8.2,4X,F8.2,4X,F8.2)
      call edisp(itu,outs)
      write(outs,580) HP,HF,FD(NTSCTR,6)
  580 format(' Relative humidity  :',F8.2,4X,F8.2,4X,F8.2)
      call edisp(itu,outs)

 9999 RETURN
      END

C *********************** TSCON2 **************************
C TSCON2 doubles the current timestep until the
C difference for zone air temperature and air node energy
C injection for previous and current timestep is smaller than
C the user-specified value.

      SUBROUTINE TSCON2(IAGAIN,ITSTP1,NTSCTR)
#include "building.h"
#include "plant.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/TS2/NF2(MSCH),IFL2(MSCH,MCVT2),FD2(MSCH,MCVT2),
     &           NMAX2(MSCH),IPASS,IPASSC,TASAV(MCOM),QASAV(MCOM)
      COMMON/TTQ/TMAH(MCOM),QMAH(MCOM)

      character outs*124

      DIMENSION DTFA(MCOM),DQFA(MCOM),TMAHC(MCOM),QMAHC(MCOM)

      IPASS=IPASS+1
      IPASSC=IPASS-1

      DO 5 L=1,NCOMP

C Find average zone air temperatue and air node energy injection
C over the hour.
        TMAHC(L)= TASAV(L)/NTSTEP
        QMAHC(L)= QASAV(L)/NTSTEP
        DTFA(L)=0.0
        DQFA(L)=0.0
    5 CONTINUE

C If first pass then save values of various quantities as
C beginning of hour values.
      IF(IPASSC.GT.0) GOTO 20
      call savepv
      IAGAIN=1
      GOTO 90

   20 CONTINUE

      IF(IPASSC.EQ.1) GOTO 25
      IF(IPASSC.EQ.NMAX2(NTSCTR)) GOTO 80
      DO 50 L=1, NCOMP
        DO 47 I=1, NF2(NTSCTR)
           II=IFL2(NTSCTR,I)
           IF (II.EQ.1) THEN
              DTFA(L)=ABS(TMAHC(L)- TMAH(L))
              DIFF=DTFA(L)
           ENDIF
           IF (II.EQ.2) THEN
              DQFA(L)=ABS(QMAHC(L)- QMAH(L))
              DIFF=DQFA(L)
           ENDIF
           IF (DIFF.GT.FD2(NTSCTR,I)) GOTO 25
   47   CONTINUE
   50 CONTINUE

      GOTO 80
   25 CONTINUE

C Save average zone air temperature and air node
C energy injection.
      DO 75 L=1, NCOMP
        TMAH(L)=TMAHC(L)
        QMAH(L)=QMAHC(L)
   75 CONTINUE

      call rtrvpv
C Double time-step for up-coming pass.
      NTSTEP=NTSTEP*2
      IAGAIN=1
      GOTO 90

   80 CONTINUE
      IAGAIN=0
      IPASS=0
      NTSTEP=ITSTP1
   90 CONTINUE

C Trace output?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)GOTO 9999
      IF(ITRACE(41).NE.1.OR.NSINC.GT.ITCF)GOTO 9999
      write(outs,'(A,I4,A,I4)')
     & ' Subroutine TSCON2     Trace output',ICNT,' Present hour',IHRP
      call edisp(itu,outs)
      call edisp(itu,' ')
      ICNT=ICNT+1

      DO 5172 J=1,NF2(NTSCTR)
        write(outs,510) IFL2(NTSCTR,J),FD2(NTSCTR,J)
  510   format(' Control variable type =',I5,9X,
     &         ' Specified difference =',F10.5)
        call edisp(itu,outs)
 5172 CONTINUE
      write(outs,517) NMAX2(NTSCTR),IPASSC
  517 format(' NMAX2  = ',I5,10X,' IPASSC = ',I5)
      call edisp(itu,outs)
      write(outs,5171) IAGAIN,NSINC,NTSTEP
 5171 format(' IAGAIN = ',I5,' NSINC  = ',I5,' NTSTEP = ',I5)
      call edisp(itu,outs)
      write(outs,515)
  515 format(26X,'DTFA(I)     DQFA(I)     TMAHC(I)      QMAHC(I)')
      call edisp(itu,outs)

C Print values of following variables for all zones
      DO 600 I=1,NCOMP
         write(outs,520) I,DTFA(I),DQFA(I),TMAHC(I),QMAHC(I)
  520    format('ZONE:',I5,10X,4(F12.5,2X))
         call edisp(itu,outs)
  600 CONTINUE

 9999 RETURN
      END

C ************************* TSCON4 *************************
C TSCON4 checks if another iteration is required for type 4 TSC.

      SUBROUTINE TSCON4(ITRFLG,NTSCTR)
#include "building.h"

      COMMON/TS4/ITSITR(MSCH),ITRPAS

C Increment iteration counter (ITRPAS) only if first 
C iteration is complete.  ITRPAS=0 for the first iteration 
C and if there is no time step control.  This is checked in several
C places in the code so that the present time row coefficients are 
C updated only once.  If ITRFLG=0 then results are saved.
      if (ITRFLG.eq.1) ITRPAS=ITRPAS+1
      ITRFLG=1

C If last iteration set ITRFLG=0 so that results are saved.
C Also set ITRPAS=0.
      if (ITRPAS.eq.ITSITR(NTSCTR)-1) then
        ITRFLG=0
      endif

      RETURN
      END

C ************************* TSCON6 *************************
C TSCON6 checks if `rewind` operation is required for type 6 TSC.

      SUBROUTINE TSCON6(idy,ihr,NTSCTR,irew)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TS/ITSFLG,NTSCPD,ITSSCH(MSCH,5),itsmus
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone
      COMMON/OPTCTL/IRWIND,IRWLAS,IRWCNT,IRWCSV,IRWCHP,IRWCHN,IRWCHG,
     &LGCTIN,INITCT,STFUT,STPRES,STPAS1,STPAS2,IRWFNL,TCTLSV

      logical rewnd,irew,tsclaw,pasone

      character outs*124
      
C RESET REWIND FLAG, `IREW`,
      irew=.false.

C POSSIBLY SAVE & RETRIEVE DATA:
C IF AT PERIOD START TIME,
      if(ihr.eq.itssch(ntsctr,1).or.(tsclaw.and.
     &(ihr.eq.1.and.itssch(ntsctr,1).eq.24.and.(nsinc.eq.1.or.pasone))))
     &then
        if(nsinc.eq.1)pasone=.true.

        if(itssch(ntsctr,1).eq.itssch(ntsctr,2).and.idy.ne.idysv)goto 75
        if(nsinc.le.nsncv1)goto 75
C REMEMBER DAY AND HOUR TO REWIND TO,
        idysv=idy
        ihrsv=ihr
C SET INITIALISATION COUNTER,
        lgctin=1        
C SAVE DATA,
        if(irwcou.eq.0)call savepv
C RETRIEVE DATA,
        if(irwcou.gt.0)call rtrvpv
C REMEMBER NSINC TO AVOID SAVING TWICE AT SAME TIME-STEP,
        nsncv1=nsinc+1
      endif

C SET REWIND FLAG:
C IF AT PERIOD STOP TIME,
75    if(ihr.eq.(itssch(ntsctr,2)))then
        
C IF MENU DRIVEN INPUT,
C (rewnd actually set elsewhere but not active yet, so temporarily ...),
        if(.not.tsclaw)rewnd=.true.

C IN THE CASE OF TSC SET UP CONTROL LAW, 
        if(itssch(ntsctr,1).eq.itssch(ntsctr,2).and.idy.eq.idysv)then
           irew=.false.
        endif

C SET REWIND FLAG INDICATING REWIND NECESSARY IN MZNUMA,
        if(rewnd.and.nsinc.eq.nsncv2)then
             irew=.true.
C INCREMENT REWIND COUNTER,
             irwcou=irwcou+1
C IF MENU DRIVEN INPUT, LIMIT NO. OF REWIND OPERATIONS,
             if(.not.tsclaw.and.irwcou.gt.irwmax)then
                irew=.false.
                irwcou=0
             endif 
        else
           irew=.false.
        endif
      endif
C REMEMBER NSINC TO AVOID INCREMENTING `IRWCOU` TWICE AT SAME TIME-STEP,
      nsncv2=nsinc+1
            
C TRACE OUTPUT ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)GOTO 9999
      IF(ITRACE(41).NE.1.OR.NSINC.GT.ITCF)GOTO 9999
      write(outs,'(A,I4)')
     & ' Subroutine TSCON6   Trace output',icnt
      call edisp(itu,outs)
      call edisp(itu,' ')
      ICNT=ICNT+1

      write(outs,'(A,I4,A,I4,A,I4)')
     & ' IDAY=',IDY,' IHR=',IHR,' NSINC=',nsinc
      call edisp(itu,outs)

      write(outs,'(A,I4,A,I4)')
     & ' IREW=',IREW,' IRWCOU=',irwcou
      call edisp(itu,outs)

      write(outs,'(A,I4,A,I4)')
     & ' IHRSV=',IHRSV,' IDYSV=',IDYSV
      call edisp(itu,outs)
      
9999  RETURN
      END

C ************************* COEFF1 *************************
C COEFF1 modifies the coefficients of arrays C and QC by the ratio of N1/M1

      SUBROUTINE COEFF1(N1,M1)
#include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      character outs*124

      TSTEP = FLOAT(N1) / FLOAT(M1)
      DO 10 I=1,NCOMP
        NC=NCONST(I)
        DO 20 J=1,NC
          NN=NNDC(I,J)
          DO 30 K=1,NN
            C(I,J,K,1)=TSTEP*C(I,J,K,1)
            C(I,J,K,2)=TSTEP*C(I,J,K,2)
            QC(I,J,K)=TSTEP*QC(I,J,K)
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE

C Trace output ?
      IF(ITC.LE.0.OR.NSINC.LT.ITC)GOTO 9999
      IF(ITRACE(42).NE.1.OR.NSINC.GT.ITCF)GOTO 9999
      write(outs,'(A,I4,A,I4)')
     & ' Subroutine COEFF1    Trace output',ICNT,'  Present hour',IHRP
      call edisp(itu,outs)
      call edisp(itu,' ')
      ICNT=ICNT+1

      write(outs,500) N1,M1,TSTEP
  500 format(' N1 = ',I5,' M1 = ',I5,' TSTEP = ',F10.5)
      call edisp(itu,outs)

 9999 RETURN
      END

C ************************* SAVEPV *************************
C SAVEPV saves present time-row variables which are
C expected to be modified at future time-row.

      subroutine savepv
#include "building.h"
#include "plant.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/COILQ/HQP,HQF,CQP,CQF
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF
      COMMON/CONVS/HCI(MCOM,MS),HCO(MCOM,MS)
      COMMON/CLIMWB/TWBP,TWBF
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP
      COMMON/CLMPHG/HEXTPP,HEXTFP,GEXTPP,GEXTFP,TWBPP,TWBFP
      COMMON/ADJCS/ATFS(MCOM,MS),ARFS(MCOM,MS)

C Note QTMCAF not yet saved!
      COMMON/COE32S/QSLIF(MCOM,MS),QSLEF(MCOM,MS),QTMCAF(MCOM,MS,MN)
      COMMON/COE33Z/QELWS(MCOM,MS)
      COMMON/COE39S/CIF(MCOM),CVF(MCOM),X3(MCOM),CVM(MCOM)
      COMMON/COE31S/HRS(MCOM,MS,MS),ZHRS(MCOM,MS)
      COMMON/COE35S/ZGEF(MCOM),ZGIF(MCOM),X1(MCOM),ZGIMF(MCOM)

      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)

C IRECPL and IRCPLP at start of the hour are required to avoid saving
C unwanted data when more than one pass is necessary.
      COMMON/LIBREC/IRECPL
      COMMON/LIBRCP/IRCPLP

      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/ZONEQS/ES(MEQ,MTR),ACAPS

      COMMON/TEMPV/TV1(40),TV2(30,MCOM),TV3(15,MCOM,MS),
     &             TV4(5,MCOM,MS,MS),TV5(2,MEQ,MTR),TV6(MCOM,MS,ME),
     &             TV7(MCOM,MS,ME,2)

      COMMON/VTHP23/THCONs(MCOM,MS,MN)
      COMMON/VTHP24/THDNSs(MCOM,MS,MHCV),THCAPs(MCOM,MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

C Save all variables intemporary array TV?.
      TV1(1) =NSINC
      TV1(2) =BTIMEF
      TV1(3) =QFF
      TV1(4) =TF
      TV1(5) =QDF
      TV1(6) =VF
      TV1(7) =DF
      TV1(8) =HF
      TV1(9) =TWBF
      TV1(10)=HEXTF
      TV1(11)=GEXTF
      TV1(12)=QFP
      TV1(13)=TP
      TV1(14)=QDP
      TV1(15)=VP
      TV1(16)=DP
      TV1(17)=HP
      TV1(18)=TWBP
      TV1(19)=GEXTP
      TV1(20)=HEXTP
      TV1(21)=QFFP
      TV1(22)=TFP
      TV1(23)=QDFP
      TV1(24)=VFP
      TV1(25)=DFP
      TV1(26)=HFP
      TV1(27)=TWBFP
      TV1(28)=GEXTFP
      TV1(29)=HEXTFP
      TV1(30)=HQP
      TV1(31)=CQP
      TV1(32)=IRECPL
      TV1(33)=IRCPLP

      DO 200 I=1,MEQ
         DO 200 J=1,MTR
            TV5(1,I,J) = E(I,J)
            TV5(2,I,J) = ES(I,J)
  200 CONTINUE

      DO 10 L=1, NCOMP
        TV2(1 ,L)=TFA(L)
        TV2(2 ,L)=QFA(L)
        TV2(3 ,L)=QFC(L)
        TV2(4 ,L)=TPA(L)
        TV2(5 ,L)=QPA(L)
        TV2(6 ,L)=QPS(L)
        TV2(7 ,L)=QPC(L)
        TV2(8 ,L)=0.0
        TV2(9 ,L)=CIF(L)
        TV2(10,L)=CVF(L)
        TV2(11,L)=X3(L)
        TV2(12,L)=CVM(L)
C 13-16 Used to be used for ZDEF,ZDIF,X2,ZDIMF
        TV2(17,L)=ZGEF(L)
        TV2(18,L)=ZGIF(L)
        TV2(19,L)=X1(L)
        TV2(20,L)=ZGIMF(L)
        TV2(21,L)=QFS(L)
        NSUR=NCONST(L)

        DO 11 I=1, NSUR
          TV3(1 ,L,I)=TPS(L,I)
          TV3(2 ,L,I)=TFS(L,I)
          TV3(4 ,L,I)=HCI(L,I)
          TV3(5 ,L,I)=HCO(L,I)
          TV3(6 ,L,I)=ATFS(L,I)
          TV3(7 ,L,I)=ARFS(L,I)
          TV3(8 ,L,I)=QSLIF(L,I)
          TV3(9 ,L,I)=QSLEF(L,I)
          TV3(10,L,I)=QELWS(L,I)
          TV3(11,L,I)=ZHRS(L,I)

          DO 13 J=1,NSUR
            TV4(1,L,I,J)=HRS(L,I,J)
   13     CONTINUE
          NN=NNDC(L,I)-1
          DO 12 J=1, NN
            TV4(4,L,I,J)=TPC(L,I,J)
            TV4(5,L,I,J)=TFC(L,I,J)
            TV6(L,I,J)=THCONs(L,I,J)
   12     CONTINUE
          NE=NELTS(L,I)
          NHCV=NN+NE
          DO 21 J=1,NHCV
            TV7(L,I,J,1)=THDNSs(L,I,J)
            TV7(L,I,J,2)=THCAPs(L,I,J)
   21     CONTINUE
   11   CONTINUE
   10 CONTINUE

      DO 30 I=1,NPNOD
        DO 31 J=1,3
          TV3(12,I,J)=CSVP(I,J)
          TV3(13,I,J)=CSVF(I,J)
   31   CONTINUE
   30 CONTINUE
      return
      end

C ************************* RTRVPV *************************
C RTRVPV retrieves all variables saved by subroutine SAVEPV.

      subroutine rtrvpv
#include "building.h"
#include "plant.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/COILQ/HQP,HQF,CQP,CQF
      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF
      COMMON/CONVS/HCI(MCOM,MS),HCO(MCOM,MS)
      COMMON/CLIMWB/TWBP,TWBF
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP
      COMMON/CLMPHG/HEXTPP,HEXTFP,GEXTPP,GEXTFP,TWBPP,TWBFP
      COMMON/ADJCS/ATFS(MCOM,MS),ARFS(MCOM,MS)
C Note QTMCAF not yet saved!
      COMMON/COE32S/QSLIF(MCOM,MS),QSLEF(MCOM,MS),QTMCAF(MCOM,MS,MN)
      COMMON/COE33Z/QELWS(MCOM,MS)
      COMMON/COE39S/CIF(MCOM),CVF(MCOM),X3(MCOM),CVM(MCOM)
      COMMON/COE31S/HRS(MCOM,MS,MS),ZHRS(MCOM,MS)
      COMMON/COE35S/ZGEF(MCOM),ZGIF(MCOM),X1(MCOM),ZGIMF(MCOM)

      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)

C IRECPL and IRCPLP at start of the hour are required to avoid saving
C unwanted data when more than one pass is necessary.
      COMMON/LIBREC/IRECPL
      COMMON/LIBRCP/IRCPLP

      COMMON/ZONEQN/E(MEQ,MTR)
      COMMON/ZONEQS/ES(MEQ,MTR),ACAPS

      COMMON/TEMPV/TV1(40),TV2(30,MCOM),TV3(15,MCOM,MS),
     &             TV4(5,MCOM,MS,MS),TV5(2,MEQ,MTR),TV6(MCOM,MS,ME),
     &             TV7(MCOM,MS,ME,2)

      COMMON/VTHP23/THCONs(MCOM,MS,MN)
      COMMON/VTHP24/THDNSs(MCOM,MS,MHCV),THCAPs(MCOM,MS,MHCV)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

C Restore values of various quantities to those at beginning
C of hour.
      NSINC  =INT(TV1(1))
      BTIMEF =TV1(2)
      QFF    =TV1(3)
      TF     =TV1(4)
      QDF    =TV1(5)
      VF     =TV1(6)
      DF     =TV1(7)
      HF     =TV1(8)
      TWBF   =TV1(9)
      HEXTF  =TV1(10)
      GEXTF  =TV1(11)
      QFP    =TV1(12)
      TP     =TV1(13)
      QDP    =TV1(14)
      VP     =TV1(15)
      DP     =TV1(16)
      HP     =TV1(17)
      TWBP   =TV1(18)
      GEXTP  =TV1(19)
      HEXTP  =TV1(20)
      QFFP   =TV1(21)
      TFP    =TV1(22)
      QDFP   =TV1(23)
      VFP    =TV1(24)
      DFP    =TV1(25)
      HFP    =TV1(26)
      TWBFP  =TV1(27)
      GEXTFP =TV1(28)
      HEXTFP =TV1(29)
      HQP    =TV1(30)
      CQP    =TV1(31)
      IRECPL =INT(TV1(32))
      IRCPLP =INT(TV1(33))

      DO 250 I=1,MEQ
         DO 250 J=1,MTR
            E(I,J)  = TV5(1,I,J)
            ES(I,J) = TV5(2,I,J)
  250 CONTINUE


      DO 60 L=1, NCOMP
        TFA(L)   =TV2(1 ,L)
        QFA(L)   =TV2(2 ,L)
        QFC(L)   =TV2(3 ,L)
        TPA(L)   =TV2(4 ,L)
        QPA(L)   =TV2(5 ,L)
        QPS(L)   =TV2(6 ,L)
        QPC(L)   =TV2(7 ,L)
        CIF(L)   =TV2(9 ,L)
        CVF(L)   =TV2(10,L)
        X3(L)    =TV2(11,L)
        CVM(L)   =TV2(12,L)
C 13-16 Used to be used for ZDEF,ZDIF,X2,ZDIMF
        ZGEF(L)  =TV2(17,L)
        ZGIF(L)  =TV2(18,L)
        X1(L)    =TV2(19,L)
        ZGIMF(L) =TV2(20,L)
        QFS(L)   =TV2(21,L)
        NSUR=NCONST(L)

        DO 61 I=1, NSUR
          TPS(L,I)   =TV3(1 ,L,I)
          TFS(L,I)   =TV3(2 ,L,I)
          HCI(L,I)   =TV3(4 ,L,I)
          HCO(L,I)   =TV3(5 ,L,I)
          ATFS(L,I)  =TV3(6 ,L,I)
          ARFS(L,I)  =TV3(7 ,L,I)
          QSLIF(L,I) =TV3(8 ,L,I)
          QSLEF(L,I) =TV3(9 ,L,I)
          QELWS(L,I) =TV3(10,L,I)
          ZHRS(L,I) =TV3(11,L,I)

          DO 63 J=1,NSUR
            HRS(L,I,J)=TV4(1,L,I,J)
   63     CONTINUE
          NN=NNDC(L,I)-1
          DO 62 J=1, NN
            TPC(L,I,J)=TV4(4,L,I,J)
            TFC(L,I,J)=TV4(5,L,I,J)
            THCONs(L,I,J)=TV6(L,I,J)
   62     CONTINUE
          NE=NELTS(L,I)
          NHCV=NN+NE
          DO 68 J=1,NHCV
            THDNSs(L,I,J)=TV7(L,I,J,1)
            THCAPs(L,I,J)=TV7(L,I,J,2)
   68     CONTINUE
   61   CONTINUE
   60 CONTINUE

      DO 70 I=1,NPNOD
        DO 71 J=1,3
          CSVP(I,J)=TV3(12,I,J)
          CSVF(I,J)=TV3(13,I,J)
   71   CONTINUE
   70 CONTINUE
      return
      end


C ************************* TSCON7 *************************

C This routine checks if another iteration is required between
C the plant and electrical solution domains.  This type of
C iteration is known as TSC type 7.  This current implementation
C is configured to support models that include a distributed
C generation plant component.  Further, it presumes that the
C DG component is controlled with subroutine DG_controller.

      SUBROUTINE TSCON7(plant_elec_converged)
#include "power.h"
#include "CETC_definitions.h" 
C------------------------------------------------------------------------------
C Common block variables.
C------------------------------------------------------------------------------

C-----Electrical load that DG unit responds to (W) 
      common/TSCON7dat/plt_elec_load,plt_elec_gen,plt_elec_balance
      real plt_elec_load, plt_elec_gen, plt_elec_balance

C-----Solution variables from electric power flow domain.
      COMMON/ENETDER/PENETIMP,QENETIMP,PENETLOD,QENETLOD,PENETGEN,
     &QENETGEN,PENETRN,QENETRN,ENETIPF,ENETGPF,ENETLPF,ENETLOS,
     &PENETPHL(MPHAS),QENETPHL(MPHAS),PENETPHG(MPHAS),
     &QENETPHG(MPHAS),PENETPHI(MPHAS),QENETPHI(MPHAS)

C------------------------------------------------------------------------------
C Type declarations for variables passed within common blocks.
C------------------------------------------------------------------------------
      REAL PENETIMP,QENETIMP,PENETLOD,QENETLOD,PENETGEN,QENETGEN,
     &PENETRN,QENETRN,ENETIPF,ENETGPF,ENETLPF,ENETLOS,PENETPHL,QENETPHL,
     &PENETPHG,QENETPHG,PENETPHI,QENETPHI

C H2-dep_controller: start
C Flag for convergence of the hydrogen/electric controller's plant
C control signals.
      common / resh2_ctl_status / bResH2_ctl_unconverged
      logical bResH2_ctl_unconverged
C H2-dep_controller: end 

C------------------------------------------------------------------------------
C Type declarations for local variables.
C------------------------------------------------------------------------------
      LOGICAL plant_elec_converged
      REAL  ele_elec_load, ele_elec_gen, ele_elec_balance
      INTEGER iter_count_PE
      SAVE iter_count_PE
      LOGICAL CLOSEa,CLOSEb,CLOSEc,CLOSE_TO_ZERO

C------------------------------------------------------------------------------
C     References
C------------------------------------------------------------------------------
      REAL elec_net_load_calc

C------------------------------------------------------------------------------
C Within the plant domain the DG unit was made to respond to an electrical
C load. This load was an estimate only: the actual electrical load is
C calculated by the electrical solution domain, which is invoked following
C convergence of the plant domain.  Compare the estimated load from the plant
C domain to that calculated by the electrical domain.  If they differ by more
C than 1 W then force another iteration through the plant and electrical
C domains.
C------------------------------------------------------------------------------

C Get the electrical load that was determined by the electric power flow domain.
C The common block variable `elecload_placed_on_DG' contains the estimated
C load from the plant domain.

      ele_elec_load    = elec_net_load_calc(total_load) ! W
      ele_elec_gen     = elec_net_load_calc(total_gen)  ! W
      ele_elec_balance = elec_net_load_calc(balance)    ! W

      CALL ECLOSE( plt_elec_load    , ele_elec_load   ,2.0,CLOSEa) ! 2 W tolerance
      CALL ECLOSE( plt_elec_gen     , ele_elec_gen    ,2.0,CLOSEb) ! 2 W tolerance
      CALL ECLOSE( plt_elec_balance , ele_elec_balance,2.0,CLOSEc) ! 2 W tolerance


C Test and debug (begin).
C      write(93,*)
C      write(93,*)'~~~ Checking elec-plant iter: '
C      write(93,*)'     - Load (plt/elec): ',plt_elec_load,ele_elec_load
C      write(93,*)'     - Gen  (plt/elec): ',plt_elec_gen,ele_elec_gen
C      write(93,*)'     - Bal  (plt/elec): ',plt_elec_balance,
C     & ele_elec_balance

     
C Test and debug (end).

C Determine whether further iteration between the plant and electrical domains
C is required, but do not iteration more than 10 times between the solution
C domains.

C H2-dep_controller:start
C Interation control for Hydrogen systems (experimental)
      IF( .not. CLOSEa .OR.
     &    .not. CLOSEb .OR.
     &    .not. CLOSEc .OR.
     &    bResH2_ctl_unconverged )THEN
C H2-dep_controller:end

C      IF( .not. CLOSEa .OR.
C     &    .not. CLOSEb .OR.
C     &    .not. CLOSEc       )THEN

         IF( iter_count_PE .lt. 10 )THEN  ! Outside tolerance, not too many iterations.
            plant_elec_converged = .FALSE.
            iter_count_PE = iter_count_PE + 1
C            write(93,*) '     + Continue iterating: iter = ',
C     &                  iter_count_PE,' ratio = ',ratio
         ELSE ! Too many iterations already
C            write(93,*) '     + Not converged but ending after iter = ',
C     &                  iter_count_PE+1,' ratio = ',ratio
            plant_elec_converged = .TRUE.
C Reset iteration counter to prepare for next time-step. 
            iter_count_PE = 0 
         ENDIF
      ELSE ! Converged.
C         write(93,*) '     + Converged after iter = ',
C     &                  iter_count_PE+1,' ratio = ',ratio
         plant_elec_converged = .TRUE.
C Reset iteration counter to prepare for next time-step. 
         iter_count_PE = 0
      ENDIF


      RETURN
      END
      
