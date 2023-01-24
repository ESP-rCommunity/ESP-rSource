C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following routines:
C    MODESQ: main controller for the design questions facility.
C    MORANK: rank ordering of zones.
C    MOEBAL: retrieval of energy balance data.

C ******************** MODESQ ********************

C Main controller for the design questions facility.

      SUBROUTINE MODESQ
#include "building.h"
#include "geometry.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/OUTPCH/ICOUT
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/ZONPIK/NZ,NZNO(MCOM)

      COMMON/DESQ0/DQA(MCOM),DQB(MCOM),DQC(MCOM),DQD(MCOM)
      COMMON/DESQ/IDESQ,IDQV1,IDQV2,IDQV3,DQV1,DQV2,DQV3

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION IRANK(MCOM),VRANK(MCOM),KDUM(MCOM),ZDUM(MCOM),
     &          LABEL(10)

      character outs*124
      CHARACTER*10 LABEL
      CHARACTER*16 ITEM(10)
      logical close
      integer NITMS,INO ! max items and current menu item

      helpinsub='design'     ! set for cfiles

      ITEM(1) ='2 result set    '
      ITEM(2) ='3 period        '
      ITEM(3) ='4 zones         '
      ITEM(4) ='  ...........   '
      ITEM(5) ='a overheating   '
      ITEM(6) ='b underheating  '
      ITEM(7) ='c energy gains  '
      ITEM(8) ='d energy losses '
      ITEM(9) ='  ...........   '
      ITEM(10)='- exit menu'

      LABEL(1)='Air infilt'
      LABEL(2)='Air ventil'
      LABEL(3)='Ex wd cond'
      LABEL(4)='In wd cond'
      LABEL(5)='Ex dr cond'
      LABEL(6)='In dr cond'
      LABEL(7)='Air solar '
      LABEL(8)='Cnv casual'
      LABEL(9)='Opaque cnv'
      LABEL(10)='Plant cap '

    3 NITMS=10
      INO=-2

C Help text for menu.
      helptopic='res_design_q_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

    4 CALL EMENU('Design questions',ITEM,NITMS,INO)

C Test for illegal menu pick.
      IF(INO.EQ.0.OR.INO.EQ.4.OR.INO.EQ.9)goto 1
      IF(ISAVE.EQ.1.AND.(INO.EQ.8.OR.(INO.GE.11.AND.INO.LE.21)))then
        call edisp(iuout,' ')
        call edisp(iuout,'The save option chosen prior to simulation')
        call edisp(iuout,'disables this command selection.')
        goto 1
      endif
      IF(INO.EQ.1)CALL MORESS
      IF(INO.EQ.2)CALL MOOPER
      IF(INO.EQ.3)CALL MOZDFN
      IF(INO.GT.4.AND.INO.LE.8)CALL MOCHEK(IER)
      IF(IER.EQ.1)goto 1
      IF(INO.EQ.10)goto 6
      IG=INO-4
      goto 8

C Offer menu reselection.
    1 INO=-1
      goto 4

C Set design questions.
    8 GOTO(11,31,51,71),IG

C Test for overheating.
   11 call edisp(iuout,' ')
      call edisp(iuout,'Design question 1: overheating')
      call edisp(iuout,' ')
      call edisp(iuout,'Critical variable: (1) air temperature,')
      call edisp(iuout,'(2) controlled temperature, (3) resultant')
      call edisp(iuout,'temperature.')

      helptopic='res_design_critical_var'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(IDQV1,' ','Critical variable? ',
     &  1,'F',3,'F',1,'design 1 critical var',IER,nbhelp)

C Solicit upper temperature limit.
      CALL EASKR(DQV1,' ','Upper temperature limit?',
     &   0.0,'-',0.0,'-',24.,'upper temp limit',IER,nbehlp)

C Set appropriate value of IGET.
      IF(IDQV1.EQ.1)IGET=3
      IF(IDQV1.EQ.2)IGET=5
      IF(IDQV1.EQ.3)IGET=4

      IDESQ=1

C Rank order all considered zones.
      CALL MORANK(DQA,IRANK,VRANK)

C Check upper temperature limit.
      NZONES=0
      DO 10 I=1,NZ
      IF(VRANK(I).GT.DQV1)NZONES=NZONES+1
   10 CONTINUE

      IF(NZONES.EQ.0)THEN
        call edisp(icout,' ')
        call edisp(icout,'Answer: no overheating occurs!')
      ELSE IF(NZONES.GT.0)THEN
        call edisp(icout,' ')
        call edisp(icout,'Answer: overheating occurs in zone(s):')
        do 19 J=1,NZONES
          write(outs,'(A)')zname(IRANK(J))(1:lnzname(IRANK(J)))
          call edisp(icout,outs)
   19   CONTINUE
      ENDIF
      IDESQ=0
      goto 3

C Test for underheating.
   31 call edisp(iuout,' ')
      call edisp(iuout,'Design question 2: underheating')
      call edisp(iuout,'Critical variable: (1) air temperature,')
      call edisp(iuout,'(2) control temperature, (3) resultant')
      call edisp(iuout,'temperature.')
      call edisp(iuout,' ')
      helptopic='res_design_critical_var'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKI(IDQV2,' ','Critical variable?',
     &  1,'F',3,'F',1,'design 2 critical var',IER,nbhelp)

C Solicit lower temperature limit.
      CALL EASKR(DQV2,' ','Lower temperature limit?',
     &   0.0,'-',0.0,'-',24.,'upper temp limit',IER,nbhelp)

C Set appropriate value of IGET.
      IF(IDQV2.EQ.1)IGET=3
      IF(IDQV2.EQ.2)IGET=5
      IF(IDQV2.EQ.3)IGET=4

      IDESQ=2

C Rank order all considered zones.
      CALL MORANK(DQB,IRANK,VRANK)

C Check upper limit.
      NZONES=0
      DO 30 I=1,NZ
      IF(VRANK(NZ-(I-1)).LT.DQV2)THEN
        NZONES=NZONES+1
        KDUM(I)=IRANK(NZ-(I-1))
      ENDIF
   30 CONTINUE

      IF(NZONES.EQ.0)THEN
        call edisp(icout,' ')
        call edisp(icout,'Answer: no underheating occurs!')
      ELSE IF(NZONES.GT.0)THEN
        call edisp(icout,' ')
        call edisp(icout,'Answer: underheating occurs in zone(s):')
        do 12 J=1,NZONES
          write(outs,'(A)')zname(KDUM(J))(1:lnzname(KDUM(J)))
          call edisp(icout,outs)
   12   CONTINUE
      ENDIF
      IDESQ=0
      goto 3

C Test for largest energy gains to zone.
   51 call edisp(iuout,' ')
      call edisp(iuout,'Design Question 3: energy gains')
      
C Obtain energy gains information.
      CALL MOEBAL
      DO 50 I=1,10
        DQA(I)=VAL2(I,1)
   50 CONTINUE
      TVAL=VAL3(1,1)

C Rank order all considered zones.
      DO 60 I=1,10
      XV=DQA(I)
      ICOUNT=1

      DO 70 J=1,10
      IF(J.NE.I)THEN
        IF(XV.LT.DQA(J))ICOUNT=ICOUNT+1
        call eclose(XV,DQA(J),0.0001,close)
        IF(close.AND.J.LT.I)ICOUNT=ICOUNT+1
      ENDIF
   70 CONTINUE

      IRANK(ICOUNT)=I
      VRANK(ICOUNT)=XV

   60 CONTINUE

C Calculate main gains to space: included only
C if > 75% of the total.
      SUM=0.0
      IK=0
   53 IK=IK+1
      SUM=SUM+VRANK(IK)
      POFTOT=SUM/TVAL
      IF(POFTOT.LT.0.75)goto 53
      call edisp(icout,' ')
      write(outs,'(3A)')'Main gains to ',zname(NZNO(1)),' are:'
      call edisp(icout,outs)
      DO 55 I=1,IK
      IR=IRANK(I)
      write(outs,56)LABEL(IR),VRANK(I)
   56 FORMAT(1X,A10,',',F12.2)
      call edisp(icout,outs)
   55 CONTINUE

      IDESQ=0
      goto 3

C Test for largest energy losses from zone.
   71 call edisp(iout,'Design Question 4: energy losses')

C Call MOEBAL to obtain gains information.
      CALL MOEBAL
      DO 80 I=1,10
      DQB(I)=VAL2(I,2)
   80 CONTINUE
      TVAL=VAL3(2,1)

C Rank order all considered zones.
      DO 81 I=1,10
      XV=DQB(I)
      ICOUNT=1

      DO 82 J=1,10
      IF(J.NE.I)THEN
        IF(XV.LT.DQB(J))ICOUNT=ICOUNT+1
        call eclose(XV,DQB(J),0.0001,close)
        IF(close.AND.J.LT.I)ICOUNT=ICOUNT+1
      ENDIF
   82 CONTINUE

      IRANK(ICOUNT)=I
      VRANK(ICOUNT)=XV

   81 CONTINUE
      DO 83 I=1,10
        ZDUM(I)=VRANK(10-(I-1))
        KDUM(I)=IRANK(10-(I-1))
   83 CONTINUE

C Calculate main losses to space. Include only
C losses > 75% of total.
      SUM=0.0
      IK=0
   73 IK=IK+1
      SUM=SUM+ZDUM(IK)
      POFTOT=SUM/TVAL
      IF(POFTOT.LT.0.75)goto 73
      call edisp(icout,' ')
      write(outs,'(3A)')' Main losses from',zname(NZNO(1)),' are:'
      call edisp(icout,outs)
      DO 75 I=1,IK
        IR=KDUM(I)
        write(outs,76)LABEL(IR),ZDUM(I)
   76   format(1X,A10,',',F12.2)
        call edisp(icout,outs)
   75 CONTINUE

      IDESQ=0
      goto 3

    6 RETURN
      END

C ******************** MORANK ********************

C Rank ordering of zones.

      SUBROUTINE MORANK(XVAL,IRANK,VRANK)
#include "building.h"

      COMMON/ZONPIK/NZ,NZNO(MCOM)

      DIMENSION XVAL(MCOM),IRANK(MCOM),VRANK(MCOM)
      logical close

      DO 10 I=1,NZ
      IZ=NZNO(I)
      XV=XVAL(IZ)
      ICOUNT=1

      DO 20 J=1,NZ
      IF(J.NE.IZ)THEN
        IF(XV.LT.XVAL(J))ICOUNT=ICOUNT+1
        call eclose(XV,XVAL(J),0.0001,close)
        IF(close.AND.J.LT.IZ)ICOUNT=ICOUNT+1
      ENDIF
   20 CONTINUE

      IRANK(ICOUNT)=IZ
      VRANK(ICOUNT)=XV

   10 CONTINUE

      RETURN
      END

C ******************** MOEBAL ********************

C Retrieval of energy balance data.

      SUBROUTINE MOEBAL
#include "building.h"

      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/ZONPIK/NZ,NZNO(MCOM)

      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      DIMENSION XVAL1(10)

      TCMULT=NOUT/(NTS*1000.)

C Determine required output zone.
      IZONE=NZNO(1)

C Initialise summation locations: VAL2(I,J) is used to store
C summations where I=1 to 10 (one for each flowpath) and
C J=1 to 2 for gain and loss values.
      DO 20 I=1,10
      DO 201 J=1,3
      VAL2(I,J)=0.
  201 CONTINUE
   20 CONTINUE

C Day-by-day recovery.
      DO 30 I=IODS,IODF
      ID=I

C For requested zone and for every computational
C time-step, get results for complete day (held in VAL1).
      CALL MOEGET(IZONE,ID)

C Stepping at defined output interval.
      N=24*NTS
      DO 40 J=1,N,NOUT
      JT=J

C Compute current time.
      call DATIME(JT,TIME)

C Within requested output period ?
      IF(I.EQ.IODS.AND.TIME.LT.IOH1)goto 40
      IF(I.EQ.IODF.AND.TIME.GT.IOH2)goto 40

C Compute average if requested.
      IF(IAV.EQ.0)goto 57

C Compute average: VAL1(I,J) holds the time-step results
C where I=1 to 10 relates to the energy flows in the order
C they appear on output.
      K1=J
      K2=J+NOUT
      DO 50 K=1,10
      XVAL1(K)=0.
   50 CONTINUE
      DO 60 L=K1,K2
      XMULT=1.
      IF(L.EQ.K1.OR.L.EQ.K2)XMULT=.5
      DO 70 M=1,10
      XVAL1(M)=XVAL1(M)+VAL1(M,L)*XMULT
   70 CONTINUE
   60 CONTINUE
      DIV=FLOAT(K2-K1)
      DO 80 M=1,10
      XVAL1(M)=XVAL1(M)/DIV
   80 CONTINUE
      goto 58

C No averaging.
   57 DO 90 M=1,10
      XVAL1(M)=VAL1(M,J)
   90 CONTINUE

C Breakdown in terms of net gain and loss: summations
C held in VAL2(I,J) where I=1 to 10 is as above and
C J=1, 2 and 3 relate to GAINS, LOSSES and NET respectively.
   58 DO 130 K=1,10
      IF(XVAL1(K).LE.0.)goto 68
      VAL2(K,1)=VAL2(K,1)+XVAL1(K)*TCMULT
      goto 129
   68 VAL2(K,2)=VAL2(K,2)+XVAL1(K)*TCMULT
  129 VAL2(K,3)=VAL2(K,1)+VAL2(K,2)
  130 CONTINUE
   40 CONTINUE
   30 CONTINUE

C Totals: use VAL3(I,1) where I=1 to 3.
      DO 170 I=1,3
      DO 180 K=1,10
      VAL3(I,1)=VAL3(I,1)+VAL2(K,I)
  180 CONTINUE
  170 CONTINUE

      RETURN
      END
