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

C      M3BCTL: control executive.
C      MZMIXT: calculate `weighted' zone temperature.
C      CF3VAR: get sensed condition for single sensor control.
C      B3CL01: ideal control.
C      B3CL02: free float.

C ******************** M3BCTL
C The main 3D building control function executive.

      SUBROUTINE M3BCTL

#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT

      character outs*124

C If actuator is not located in zone then no control is imposed
C regardless of control function prevailing at this time.
      IF(IBAN(ICF,1).EQ.-1.OR.(IBAN(ICF,1).EQ.-2.AND.IBAN(ICF,2).NE.
     &IICOMP).OR.(IBAN(ICF,1).GT.0.AND.IBAN(ICF,1).NE.IICOMP))GOTO 2

C For the special case of current zone, mixed node control only.
      IF(IBSN(ICF,1).EQ.-2.AND.IBSN(ICF,2).EQ.IICOMP)THEN
        STOP 'error (01) in M3BCTL'
      ENDIF

C 14 control functions active at present + thermo-physical
C property substitution (BCL99).  If control function index
C negated then property substitution active within zone
C associated with control function.
      IFUNC=ABS(IBCLAW(ICF,IDTYP,IPER))
      GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14),IFUNC
      GOTO 98

C CL1 is ideal control with potential for a different heating and
C cooling set point.   Limiting capacities allow either heating or
C cooling to be disallowed.
    1 CALL B3CL01
      GOTO 99

C CL2 is a free-floating controller.
    2 CALL B3CL02
      GOTO 99

C CL3 is an ideal pre-heating/pre-cooling condition. N/A.
    3 STOP 'error (03) in M3BCTL'

C CL4 is a fixed heat injection or extraction period. N/A.
    4 STOP 'error (04) in M3BCTL'

C CL5 is a three-term PID controller. N/A.
    5 STOP 'error (05) in M3BCTL'

C CL6 allows zone input or extract flux as function of plant network status. N/A.
    6 STOP 'error (06) in M3BCTL'

C CL7 allows a multi-stage energy supply to be defined. N/A.
    7 STOP 'error (07) in M3BCTL'

C CL8 is a variable supply temperature system. N/A.
    8 STOP 'error (08) in M3BCTL'

C CL9 is a heat pipe. N/A.
    9 STOP 'error (09) in M3BCTL'

C CL10 allows separate flux ON and flux OFF set points. N/A.
   10 STOP 'error (10) in M3BCTL'

C CL11 is a multi-sensor controller. N/A.
   11 STOP 'error (11) in M3BCTL'

C CL12 is a multi-sensor on/off controller. N/A.
   12 STOP 'error (12) in M3BCTL'

C CL13 is a time-proportioning on/off controller. N/A.
   13 STOP 'error (13) in M3BCTL'

C CL14 is a floating ('three-position') controller. N/A.
   14 STOP 'error (14) in M3BCTL'

C New control laws should be inserted here.
   98 write(outs,97)IFUNC
   97 format(' Building control function',I3,' is not active in')
      call edisp(iuout,outs)
      call edisp(iuout,' this release but has been referenced: ')
      call edisp(iuout,' this is a fatal error.')
      close(ieout)
      CALL ERPFREE(ieout,ISTAT)
      call epwait
      call epagend
      STOP

C CL99 will modify the thermo-physical properties of
C the specified multi-layered constructions.
   99 ifunc=ibsn(icf,4)
      if(ifunc.gt.0)STOP 'error (99) in M3BCTL'
      RETURN
      END

C ******************** CF3VAR

C CF3VAR determines the sensed condition associated
C with the sensor defined for the control function.

      SUBROUTINE CF3VAR(TCTL)

#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      IF(IBSN(ICF,1).EQ.0.OR.IBSN(ICF,1).EQ.IICOMP)THEN
         TCTL=TNP

      ELSEIF(IBSN(ICF,1).GT.0.AND.IBSN(ICF,1).NE.IICOMP)THEN
         IZ=IBSN(ICF,1)
         IS=IBSN(ICF,2)
         IN=IBSN(ICF,3)
         IF(IN.EQ.0.AND.IS.EQ.0)TCTL=TFA(IZ)
         IF(IN.EQ.0.AND.IS.GT.0)TCTL=TFS(IZ,IS)
         IF(IN.GT.0.AND.IS.GT.0)TCTL=TFC(IZ,IS,IN)

C Sensor measures plant node state-space variable.
      ELSE IF(IBSN(ICF,1).EQ.-1) THEN

C Which is obviously not allowed in case of bld.
         call edisp(iuout,
     &   'CF3VAR: simulator does not support sensing plant node state.')
         close(ieout)
         CALL ERPFREE(ieout,ISTAT)
         call epwait
         call epagend
         STOP
      ELSEIF(IBSN(ICF,1).EQ.-2)THEN
        STOP 'error (01) in CF3VAR'

      ELSEIF(IBSN(ICF,1).EQ.-3.AND.IBSN(ICF,2).EQ.0)THEN
         TCTL=TF

      ELSEIF(IBSN(ICF,1).EQ.-3.AND.IBSN(ICF,2).EQ.1)THEN
         TCTL=SOLAIR(TF,QFF,QDF)
      ENDIF

      RETURN
      END

C ******************** B3CL01

C An ideal controller.

      SUBROUTINE B3CL01

#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT

C Fatal error test.
      IF(BMISCD(ICF,IDTYP,IPER,1).LT.6.)GOTO 99
      QFUT=0.
      TFUT=TNP

C Determine sensed temperature.
      CALL CF3VAR(TCTL)

C QHM = max heating capacity (W)
C QHN = min heating capacity (W)
C QCM = max cooling capacity (W)
C QCN = min cooling capacity (W)
C HSP = heating set point temp. (deg.C)
C CSP = cooling set point temp. (deg.C)

      QHM=BMISCD(ICF,IDTYP,IPER,2)
      QHN=BMISCD(ICF,IDTYP,IPER,3)
      QCM=-BMISCD(ICF,IDTYP,IPER,4)
      QCN=-BMISCD(ICF,IDTYP,IPER,5)
      HSP=BMISCD(ICF,IDTYP,IPER,6)
      CSP=BMISCD(ICF,IDTYP,IPER,7)

      IF(TCTL.LE.CSP.AND.TCTL.GE.HSP)RETURN

C Since sensed temperature is not within range, attempt to control
C THIS zone's temperature.
      TCTL=TNP

C Is temp. too high ?
      IF(TCTL.GT.CSP)THEN
        QFUT=(BB3-BB1*CSP)/BB2

C Is this available ?
        IF(QFUT.LT.QCM)THEN
          QFUT=QCM
        ELSEIF(QFUT.GT.QCN)THEN
          QFUT=QCN
        ENDIF

C Too low, heat to HSP.
      ELSE
        QFUT=(BB3-BB1*HSP)/BB2

C Is this capacity available ?
        IF(QFUT.GT.QHM)THEN
          QFUT=QHM
        ELSEIF(QFUT.LT.QHN)THEN
          QFUT=QHN
        ENDIF
      ENDIF
      TFUT=(BB3-BB2*QFUT)/BB1
      IF(IBAN(ICF,1).EQ.-2)STOP 'error (01) in B3CL01'
      RETURN
   99 call edisp(iuout,' B3CL01: data incomplete.')
      close(ieout)
      CALL ERPFREE(ieout,ISTAT)
      call epwait
      call epagend
      STOP
      END

C ******************** B3CL02

C A free-float controller.

      SUBROUTINE B3CL02

      integer ICF,IDTYP,IPER,IICOMP
      real BB1,BB2,BB3,TNP,QFUT,TFUT
      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT

      QFUT=0.
      TFUT=TNP
      RETURN
      END
