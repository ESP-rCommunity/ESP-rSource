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

C ***************  MFSBLD
C This file contains subroutine MFLW2B.

C ************** MFLW2B 
C Transfers network flow results to bps in the form of
C zone ventilation conductances (W/K).

C mfs variables are described in emfnetw.F.
C The local variables used in this subroutine are as follows.
C     AMFR   - air mass flow rate (kg/s); AMFR(I,J) is 'positive' air
C              flow from node I to node J, i.e. only receiving nodes
C              are considered; AMFR(J,J) is flow of outside to
C              node J (node I is assumed outside when NDTYP(I)=2 and
C              SUPNOD(I,2)=1; or when NDTYP(I)=3).
C     INDX   - pivoting information in case of LU-decomposition.
C     PCOF   - Jacobian matrix coefficients.
C     TNDL   - temperature of each node in the network.
C     XNOD   - water vapour content of each node in the network.
C     TRHS   - right hand side vector of matrix equation: PCOF . TNDL = TRHS
C     XRHS   - right hand side vector of matrix equation: PCOF . XNOD = XRHS

      SUBROUTINE MFLW2B

#include "building.h"
#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/TC/ITC,ICNT

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/BTIME/BTIMEP,BTIMEF
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/CLIMHG/HEXTP,HEXTF,GEXTP,GEXTF

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

C Variables used to transport computed infiltration, ventilation
C conductances to building domain      
      COMMON/AfnToBldData/fAfnCondInfil,fAfnCondVent,iAfnIndex,
     &                    fAfnCondNorm, bAfnZoneActive,fZoneMassBalance
     
      real fAfnCondInfil(MCOM)  ! Ventilation conductance (infiltration)
      real fAfnCondVent(MCOM)   ! Ventilation conductance (zone coupled)
      real fAfnCondNorm(MCOM)   ! fAfnCondVent(MCOM) after normalisation
                                ! to external temperature.
      integer iAfnIndex(MCOM)
      logical bAfnZoneActive(MCOM)
      real fZoneMassBalance(MCOM,4)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Zone moisture balance information.
      COMMON/FVALG/GFA(MCOM)
      COMMON/ZCVNT/ZCVENT(MCOM)
C Hourly ventilation conductances and temperature of incoming air.
!      COMMON/PREC5N/CVIN(MDTY,MCOM,MT),CVVN(MDTY,MCOM,MT),
!     &ICCN(MDTY,MCOM,MT),TIAN(MDTY,MCOM,MT)
!      INTEGER ICCN
!      REAL CVIN,CVVN,TIAN

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      PARAMETER (SMALL=1.0E-15)
      CHARACTER LAPROB*72,outs*124
      logical close

      DIMENSION        AMFR(MNOD,MNOD),INDX(MNOD)
      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      DOUBLE PRECISION PCOF(MNOD,MNOD),TNDL(MNOD),TRHS(MNOD),
     &                                 XNOD(MNOD),XRHS(MNOD)
      REAL CVI,CVV,CVVM
C      INTEGER IDAY,IDTY  ! uncomment if code near line 285 changes

      COMMON/zfluid/znotair(mcom),zfldK,zfldD,zfldC,zfldA,
     &              zSWAp(mcom),zSWAf(mcom)
      logical znotair

C Initialise AMFR.
      DO 12 JNOD=1,NNOD
        DO 10 INOD=1,NNOD
          AMFR(INOD,JNOD)=0.
   10   CONTINUE
   12 CONTINUE

C Sum flows to each node; we are only interested in receiving zones
C of connections with the same fluid type.
      DO 20 ICNN=1,NCNN
        if(znotair(ITPCON(ICNN)).and.
     &     INT(SUPCMP(ITPCON(ICNN),1)).NE.2)goto 20
        if(.not.znotair(ITPCON(ICNN)).and.
     &     INT(SUPCMP(ITPCON(ICNN),1)).NE.1)goto 20
        IF(FLW1(ICNN).GT.0.) THEN
          IDSND=NODPS(ICNN)
          IDREC=NODNE(ICNN)
        ELSE
          IDSND=NODNE(ICNN)
          IDREC=NODPS(ICNN)
        END IF
        IF((NDTYP(IDSND).EQ.2.AND.INT(SUPNOD(IDSND,2)).EQ.1)
     &      .OR.NDTYP(IDSND).EQ.3) THEN
          AMFR(IDREC,IDREC)=AMFR(IDREC,IDREC)+real(ABS(FLW1(ICNN)))
        ELSE
          AMFR(IDSND,IDREC)=AMFR(IDSND,IDREC)+real(ABS(FLW1(ICNN)))
        END IF
        IF(FLW2(ICNN).GT.0.) THEN
          IDSND=NODPS(ICNN)
          IDREC=NODNE(ICNN)
        ELSE IF(FLW2(ICNN).LT.0.) THEN
          IDSND=NODNE(ICNN)
          IDREC=NODPS(ICNN)
        ELSE
          GOTO 20
        END IF
        IF((NDTYP(IDSND).EQ.2.AND.INT(SUPNOD(IDSND,2)).EQ.1)
     &     .OR.NDTYP(IDSND).EQ.3) THEN
          AMFR(IDREC,IDREC)=AMFR(IDREC,IDREC)+real(ABS(FLW2(ICNN)))
        ELSE
          AMFR(IDSND,IDREC)=AMFR(IDSND,IDREC)+real(ABS(FLW2(ICNN)))
        END IF
   20 CONTINUE

C To calculate the zone ventilation conductances and moisture balance,
C we first need to know the nodal temperatures, TNDL, and moisture
C content, XNOD, including network nodes not corresponding to a
C building zone. We assume that temperature and water vapour are
C distributed through the network in proportion to the mass flows
C (i.e. disregarding any storage effects, which are handled by BMATSU/V
C and MZVAPC respectively).

C For each node, a linear equation is established:
C TNDL(J) = SUM{ AMFR(I,J) TNDL(I)} / SUM{ AMFR(I,J)} and
C XNOD(J) = SUM{ AMFR(I,J) XNOD(I)} / SUM{ AMFR(I,J)} or
C TNDL(J) = temperature of ambient air or building zone
C XNOD(J) = moisture content of ambient air or building zone
C which can be combined into the matrix equations:
C PCOF.TNDL = TRHS  and  PCOF.XNOD = XRHS
C Set up the coefficients matrix PCOF and the right hand
C sides, TRHS and XRHS (in the case of a 'known' temperature
C node, TNDL will be equal to TNOD).

      DO 35 INOD=1,NNOD

C Zeroize arrays.
      XNOD(INOD)=0.0
      TRHS(INOD)=0.0
      XRHS(INOD)=0.0
      DO 30 JNOD=1,NNOD
         PCOF(INOD,JNOD)=0.0
   30 CONTINUE

C If not an air node.
      IF(NDFLD(INOD).NE.1) THEN
         PCOF(INOD,INOD)=DBLE(1.0)
         TRHS(INOD)=TNOD(INOD)
         XRHS(INOD)=0.0
         GOTO 35
      END IF

C If an ambient air node, temperature and moisture content known.
      call eclose(SUPNOD(INOD,2),1.00,0.0001,close)
      IF((NDTYP(INOD).EQ.2.AND.close).OR.NDTYP(INOD).EQ.3) THEN
         PCOF(INOD,INOD)=DBLE(1.0)
         TRHS(INOD)=TNOD(INOD)
         XRHS(INOD)=GEXTF
         GOTO 35
      END IF

C If a boundary node, but not an ambient air node, then temperature
C is known and moisture content is set to zero.
      call eclose(SUPNOD(INOD,2),0.00,0.0001,close)
      IF(NDTYP(INOD).EQ.2.AND.close) THEN
         PCOF(INOD,INOD)=DBLE(1.0)
         TRHS(INOD)=TNOD(INOD)
         XRHS(INOD)=0.0
         GOTO 35
      END IF

C If node corresponds to a building zone, set temperature to zone
C temperature and moisture content to GFA (water vapour content
C at the future time-row).
      DO 32 ICOMP=1,NCOMP
      IF(ICAAS(ICOMP).EQ.INOD) THEN
         PCOF(INOD,INOD)=DBLE(1.0)
         TRHS(INOD)=TNOD(INOD)
         XRHS(INOD)=GFA(ICOMP)
         GOTO 35
      END IF
   32 CONTINUE

C If none of the previous cases, establish PCOF and T/XRHS from mass
C flows at last iteration.
      DO 34 JNOD=1,NNOD
      IF(AMFR(JNOD,INOD).GT.0.0) THEN
         PCOF(INOD,INOD)=PCOF(INOD,INOD)+AMFR(JNOD,INOD)
         IF(JNOD.NE.INOD) THEN
            PCOF(INOD,JNOD)=PCOF(INOD,JNOD)-AMFR(JNOD,INOD)
         ELSE
            TRHS(INOD)=AMFR(INOD,INOD)*TF
            XRHS(INOD)=AMFR(INOD,INOD)*GEXTF
         END IF
      END IF
   34 CONTINUE

C It is possible that a node does not receive any mass flow; in this
C case, zeroise node temperature and moisture content.
      call eclose(real(PCOF(INOD,INOD)),0.00,0.00001,close)
      IF(close) THEN
         PCOF(INOD,INOD)=DBLE(1.0)
         TRHS(INOD)=TNOD(INOD)
         XRHS(INOD)=0.0
      END IF
   35 CONTINUE

C Now solve TNDL/XNOD from matrix equations PCOF . TNDL/XNOD = T/XRHS
      CALL LUDCMP(PCOF,NNOD,MNOD,INDX,D)
      CALL LUBKSB(PCOF,NNOD,MNOD,INDX,TRHS)
      CALL LUBKSB(PCOF,NNOD,MNOD,INDX,XRHS)
      DO 36 INOD=1,NNOD
      TNDL(INOD)=TRHS(INOD)
      XNOD(INOD)=XRHS(INOD)
   36 CONTINUE

C Calculate infiltration and ventilation conductances
C depending on defined fluid flow network and (local) air
C temperatures.
      DO 49 ICOMP=1,NCOMP
        IF(ICAAS(ICOMP).NE.0) THEN
          JNOD=ICAAS(ICOMP)
          bAfnZoneActive(ICOMP) = .true. 
        ELSE
          bAfnZoneActive(ICOMP) = .false. 
          GOTO 49
        END IF

C Assign fluid specific heat.
        CP=1006.0                   ! air
        if(znotair(ICOMP))CP=zfldC  ! alternative zone fluid

C Zone infiltration conductance.
        CVI=AMFR(JNOD,JNOD)*CP

C Sum zone coupling ventilation conductances.
C Normalise ventilation conductance to external
C temperature to facilitate subsequent matrix 
C processing, i.e. ventilation is treated as if
C it were infiltration.  Also, when the temperature
C of the sending node equals the outside temperature
C then assume this is also infiltration by adjusting
C the infiltration load.
        CVV=0.0
        CVVM=0.0
        DO 40 INOD=1,NNOD
          call eclose(AMFR(INOD,JNOD),0.00,0.001,close)
          IF(INOD.EQ.JNOD.OR.close) GOTO 40
            CVV=CVV+AMFR(INOD,JNOD)*CP
            IF(ABS(TF).GT.DBLE(SMALL)) THEN
              CVVM=CVVM+AMFR(INOD,JNOD)*CP*REAL(TNDL(INOD))/TF
            ELSE
              CVVM=CVVM+AMFR(INOD,JNOD)*CP*REAL(TNDL(INOD))
            ENDIF
   40   CONTINUE

C Following code no longer required because transfer of flows
C to building-side equations is now via COMMON/AfnToBldData/.
!C Now overwrite appropriate COMMON block PREC5 variable.
!C Jump depending on day of week.
!        IDAY=IDYP
!        IF(IHRF.EQ.1)IDAY=IDYP+1
!        IF(IDAY.GT.365)IDAY=IDAY-365
!        IDTY=ICALENDER(IDAY)
!        IF(CALENAME(1:7).EQ.'UNKNOWN')THEN
!
!C Day types are legacy (weekday/saturday/sunday)
!          IF(IHRF.EQ.1) GOTO 42
!          GOTO (44,44,44,44,44,45,46) IDWP
!   42     GOTO (44,44,44,44,44,45,46) IDWF
!   44     IDTY=1
!          GOTO 409
!   45     IDTY=2
!          GOTO 409
!   46     IDTY=3
!        ENDIF
C Now overwrite appropriate COMMON block PREC5 variable
!409    CVIN(IDTY,ICOMP,IHRF)=CVI
!       CVVN(IDTY,ICOMP,IHRF)=CVV
!       ICCN(IDTY,ICOMP,IHRF)=0
!       TIAN(IDTY,ICOMP,IHRF)=CVVM 

C Store zone conductances in COMMON/AfnToBldData/ for transfer
C to building. 
        fAfnCondInfil(iComp) = CVI
        fAfnCondVent(iComp)  = CVV
        iAfnIndex(iComp)     = 0
        fAfnCondNorm(iComp)  = CVVM

 49   CONTINUE

C Check if there is a corresponding mass flow node
C and if so fill the zone moisture balance array.
      DO 55 ICOMP=1,NCOMP
      fZoneMassBalance(ICOMP,1)=0.0
      fZoneMassBalance(ICOMP,2)=0.0
      fZoneMassBalance(ICOMP,3)=0.0
      fZoneMassBalance(ICOMP,4)=0.0
      IF(ICAAS(ICOMP).NE.0) THEN
         INOD=ICAAS(ICOMP)

C Infiltration.
         fZoneMassBalance(ICOMP,1)=AMFR(INOD,INOD)
         fZoneMassBalance(ICOMP,4)=AMFR(INOD,INOD)

C Check for intra-node fluid flows. ZCVNT is used elsewhere to help
C in the checking of zone moisture balance information.
         DO 52 JNOD=1,NNOD
         IF(JNOD.NE.INOD.AND.AMFR(JNOD,INOD).GT.0.0) THEN
            ZCVENT(ICOMP)=ZCVENT(ICOMP)+AMFR(JNOD,INOD)
            fZoneMassBalance(ICOMP,2)= fZoneMassBalance(ICOMP,2)
     &                                +AMFR(JNOD,INOD)*real(XNOD(JNOD))
            fZoneMassBalance(ICOMP,4)= fZoneMassBalance(ICOMP,4)
     &                                +AMFR(JNOD,INOD)
         END IF
   52    CONTINUE
      END IF
   55 CONTINUE

C Trace output.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(25).NE.0) THEN
         WRITE(outs,'(A,I3)') ' Subroutine MFLW2B    Trace output',ICNT
         call edisp(itu,outs)
         ICNT=ICNT+1
         CALL DAYCLK(IDYP,BTIMEF,ITU)

C Output computed results.
         call edisp(itu,' Node  AMFR ')
         DO 9992 I=1,NNOD
         WRITE(outs,19992) I,(AMFR(I,J),J=1,MIN(9,NNOD))
         call edisp(itu,outs)
19992    FORMAT(1X,I4,9F8.5)
 9992    CONTINUE
         DO 9993 I=1,NNOD
         WRITE(outs,19994) I,TNOD(I),TNDL(I),XNOD(I)
19994    FORMAT(' Node: ',I3,'   Tnode = ',F9.5,'   Tloc. = ',F9.5,
     &                       '   Xnode = ',F9.5)
         call edisp(itu,outs)
 9993    CONTINUE
      END IF

      RETURN
      END
