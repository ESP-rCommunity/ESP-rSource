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

C **************** MFSPLT 
C This file contains the following modules which are mfs's specific
C counterparts to be used by bps and plt: MFLW2P MFCNTL

C ***************** MFLW2P 
C Transfers mass flow results to the main energy simulation modules
C by assigning 1st phase fluid mass flows to plant component inter-
C connections (ie. over-writing of the appropriate CONVAR data).
C This routine also checks whether flows are directed as expected
C (ie. the coefficient generators implicitly assume a connection's
C  mass flow rate >= 0)

      SUBROUTINE MFLW2P

#include "building.h"
#include "net_flow.h"
#include "plant.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine MFLW2P'

C For each plant component inter-connection, over-write
C the 1st phase mass flow rate data in CONVAR and adjust
C 2nd phase mass flow rate to keep humidity ratio as before.
      DO 10 IPCON=1,NPCON
      IF(ICFFS(IPCON).ne.0) then
        IF(abs(CONVAR(IPCON,2)).lt.0.0000001) THEN
           HRATIO=0.
        ELSE
           HRATIO=CONVAR(IPCON,3)/CONVAR(IPCON,2)
        END IF
        CONVAR(IPCON,2)=real(FLW1(ICFFS(IPCON))+FLW2(ICFFS(IPCON)))
        CONVAR(IPCON,3)=HRATIO*CONVAR(IPCON,2)

C Now check direction of flow reverse flow reported to trace.
        IF(CONVAR(IPCON,2).LT.0.) THEN
          IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &      ITRACE(40).NE.0)THEN
             CALL DAYCLK(IDYP,PTIMEF,ITU)
             WRITE(ITU,*) ' MFLW2P: reverse mass flow connection ',IPCON
             WRITE(ITU,*) '         1st phase mass flow reset to zero'
          ENDIF
          CONVAR(IPCON,2)=0.
        END IF
      END If

C Next connection.
   10 CONTINUE

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) THEN
         WRITE(ITU,*) ' Subroutine MFLW2P      Trace output ',ICNT
         ICNT=ICNT+1
         CALL DAYCLK(IDYP,PTIMEF,ITU)

C Dump CONVAR array.
         WRITE(ITU,*) ' Plant connections mfs adjusted mass flow rates:'
         DO 900 IPCON=1,NPCON
         WRITE(ITU,*) IPCON,': ',CONVAR(IPCON,2),CONVAR(IPCON,3)
  900    CONTINUE
         IF(ITU.EQ.IUOUT) CALL EPAGEW
      END IF

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine MFLW2P'

      RETURN
      END

C *** MFCNTL ***********************************************************
C Finds the sensed value SVCTL from ICFP-th defined plant control loop
C based on most recent results

      SUBROUTINE MFCNTL(SVCTL)

#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      COMMON/PTIME/PTIMEP,PTIMEF

      COMMON/PCLSOL/ICFP,IDTYPP,IPERP
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,NIN

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Entering subroutine MFCNTL'

C Return if no plant control loops active
      IF(NCL.EQ.0) GOTO 999

C Determine year day number of present day
      IF(IHRP.NE.24) THEN
         IDAY=IDYP
      ELSE
         IDAY=IDYF
      END IF

C Process plant control loop ICFP to test if it is active
C Set up day type and period pointers
      NDAYT=NPCDT(ICFP)

C If NDAYT=0 set data to weekday, saturday and sunday format.
C << would need to change if there are default holidays >>
      NIN=0
      IF(NDAYT.EQ.0)THEN
        NDAYT=NBDAYTYPE
        NIN=-1*NBDAYTYPE
      ENDIF
      DO 10 IDTYPP=1,NDAYT
        IDS=IPCDV(ICFP,IDTYPP,1)
        IDF=IPCDV(ICFP,IDTYPP,2)
        IF(IDAY.GE.IDS.AND.IDAY.LE.IDF) GOTO 20
   10 CONTINUE
      WRITE(IUOUT,*) ' MFCNTL: plant control loop ',ICFP
      WRITE(IUOUT,*) '         no valid day type for year-day ',IDAY
      STOP ' MFCNTL: cannot locate appropriate day type'
        
C Check number of periods in each day and the start and finish times
   20 if(NIN.LE.-1.OR.ndayt.lt.1)idtypp=icalender(iday)
      NDAYP=NPCDP(ICFP,IDTYPP)
      IF(NDAYP.EQ.0) STOP ' MFCNTL: no day-periods defined'
      DO 22 IDAYP=1,NDAYP
      IPERP=IDAYP
      TPS=TPCPS(ICFP,IDTYPP,IDAYP)
      IF(IDAYP.LT.NDAYP) THEN
         TPF=TPCPS(ICFP,IDTYPP,IDAYP+1)
      ELSE
         TPF=24.
      END IF
      IF(PTIMEF.GT.TPS.AND.PTIMEF.LE.TPF) GOTO 30
   22 CONTINUE
      STOP ' MFCNTL: cannot locate appropriate day-period'

C Valid period established; now find current sensed value
   30 CALL CLVAR(SVCTL,IPCMP,IPNOD)

C Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(40).NE.0) WRITE(ITU,*) ' Leaving subroutine MFCNTL'
C    
  999 RETURN
      END
