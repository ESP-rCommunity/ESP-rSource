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

C *************  MFRSET 
C This subroutine updates the simulation time counters
C Checks if past LAST hour and if not:
C   reads/sets weather data,
C   calculates boundary nodes temperature and wind pressure,
C   calculates nodes fluid density,
C   calculates connections stack pressure,
C   shows simulation progress.
      SUBROUTINE MFRSET(IFCLM,LAST)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW5/RHON(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFCALC/IPSMOD,MSLVTP

      DOUBLE PRECISION FLW1,FLW2,HDP,HDV,HFL,PRES,PSTACK,RESID,SAFLW

      LOGICAL   LAST
      DIMENSION IDIF(24),ITMP(24),IDNR(24),IVEL(24),IDIR(24),IHUM(24)

C Increment time by one hour and reset various time counters
      IRH=IRH+1
      IHOUR=IHOUR+1
C Increment day if hour>24
C If end of run, set LAST=1 and return to calling module
C Recalculate day and month
      IF(IRH.GT.24) THEN
         IRH=1
         IYD=IYD+1
         IF(IYD.GT.ILYD) THEN
           LAST=.TRUE.
           GOTO 100
         END IF
         CALL EDAYR(IYD,IRD,IRM)
      END IF
C Set time
      FLWTIM=FLOAT(IRH)

C If first hour of the day read one 24 hours weather data record
      IF(IRH.EQ.1) READ(IFCLM,REC=IYD,ERR=999)
     &                 (IDIF(I),ITMP(I),IDNR(I),IVEL(I),IDIR(I),IHUM(I),
     &                  I=1,24)

C Set climate data
      DRYB=FLOAT(ITMP(IRH))/10.
      QDIF=FLOAT(IDIF(IRH))
      QDNR=FLOAT(IDIR(IRH))
      IRVH=IHUM(IRH)
      WDIR=FLOAT(IDIR(IRH))
      WSPD=WRED*FLOAT(IVEL(IRH))/10.
C
C Set temperatures of boundary nodes (type 2 & 3), 'temperature coupled'
C nodes and wind pressures (type 3)
      DO 22 INOD=1,NNOD
      IF((NDTYP(INOD).EQ.2.AND.NINT(SUPNOD(INOD,2)).EQ.1).OR.
     &    NDTYP(INOD).EQ.3) TNOD(INOD)=DRYB
      IF(ITND(INOD).GT.0) TNOD(INOD)=TNOD(ITND(INOD))
      IF(NDTYP(INOD).EQ.3) THEN
C        Set wind direction index 1-16 corresponding to intervals
C        with mid-points at 0, 22.5, ... , 337.5 deg, relative to
C        surface azimuth (i.e. wind normal to surface = index 1)
         IWDR=NINT((WDIR-SUPNOD(INOD,2))/22.5)+1
         IF(IWDR.LT.1 ) IWDR=IWDR+16
         IF(IWDR.GT.16) IWDR=IWDR-16
C        Calculate wind pressure and
C        include stack pressure (based on P(height 0)=0 )
         CD=FPRE(IWDR,NINT(SUPNOD(INOD,1)))
         RO=DENSIT(1,DRYB)
         PRES(INOD)=CD*.5*RO*WSPD*WSPD-9.81*RO*HNOD(INOD)
      END IF
   22 CONTINUE
C
C Calculate node fluid densities
      DO 32 INOD=1,NNOD
      RHON(INOD)=DENSIT(NDFLD(INOD),TNOD(INOD))
   32 CONTINUE
C
C Calculation of connections stack pressure difference based on:
C     if IPSMOD=1 most recently computed flow direction
C     if IPSMOD=2 average value of 'connected fluid densities'
      DO 42 ICNN=1,NCNN
      IDPS=NODPS(ICNN)
      IDNE=NODNE(ICNN)
      IF(IPSMOD.EQ.1) THEN
         IF(FLW1(ICNN).GE.0.0) THEN
            PSTACK(ICNN)=9.81*(RHON(IDPS)*(HNOD(IDPS)-HNOD(IDNE))
     &                         +HGTNE(ICNN)*(RHON(IDNE)-RHON(IDPS)))
         ELSE
            PSTACK(ICNN)=9.81*(RHON(IDNE)*(HNOD(IDPS)-HNOD(IDNE))
     &                         +HGTPS(ICNN)*(RHON(IDNE)-RHON(IDPS)))
         END IF
      ELSE IF(IPSMOD.EQ.2) THEN
         PSTACK(ICNN)=9.81*0.5*
     &                ((RHON(IDPS)+RHON(IDNE))*(HNOD(IDPS)-HNOD(IDNE))
     &               +(HGTNE(ICNN)+HGTPS(ICNN))*(RHON(IDNE)-RHON(IDPS)))
      ELSE
         STOP ' MFRSET: unresolvable error !'
      END IF
   42 CONTINUE
C
C Show simulation progress; but only once per RATIO indicator
      RATIO=FLOAT(IHOUR)/FLOAT((ILYD-IFYD+1)*24)
 1050 FORMAT(' Simulation ',I3,'% complete')
      IF(RATIO.GT.0.95.AND.IPROG.LT.4) GOTO 51
      IF(RATIO.GT.0.75.AND.IPROG.LT.3) GOTO 52
      IF(RATIO.GT.0.50.AND.IPROG.LT.2) GOTO 53
      IF(RATIO.GT.0.25.AND.IPROG.LT.1) GOTO 54
      GOTO 59
   51 WRITE(IUOUT,1050) 95
      GOTO 55
   52 WRITE(IUOUT,1050) 75
      GOTO 55
   53 WRITE(IUOUT,1050) 50
      GOTO 55
   54 WRITE(IUOUT,1050) 25
   55 IPROG=IPROG+1
   59 CONTINUE
C
C
  100 RETURN
C
C Error trap on read error
  999 CALL ELINC(2)
      WRITE(IUOUT,9991)
 9991 FORMAT(' MFRSET: climate file read error !')
      STOP ' MFRSET: unresolvable error !'
C
      END
C
